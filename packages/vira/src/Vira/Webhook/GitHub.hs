{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub App webhook handler as WAI sub-app

This module provides a standalone WAI application for handling GitHub webhook events.
It is decoupled from the main Vira web server and can be mounted as middleware.
-}
module Vira.Webhook.GitHub (
  webhookMiddleware,
) where

import Colog (Severity (..))
import Effectful (Eff, runEff)
import Effectful.Colog.Simple (log, tagCurrentThread)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Git (BranchName (..), CommitID (..), RepoName (..))
import Effectful.Reader.Dynamic qualified as Eff
import GitHub.Data.Webhooks.Events (
  PullRequestEvent (..),
  PullRequestEventAction (..),
  PushEvent,
 )
import GitHub.Data.Webhooks.Payload (HookPullRequest (..), HookRepository (..), HookSimpleUser (..), HookUser (..), PullRequestTarget (..))
import Network.Wai (Middleware, pathInfo)
import Servant
import Servant.GitHub.Webhook (GitHubEvent, GitHubKey (..))
import Servant.Server.Generic (AsServer, genericServeTWithContext)
import Vira.App (AppStack, GlobalSettings, ViraRuntimeState, runApp)
import Vira.App.CLI (WebSettings (..))
import Vira.CI.Client qualified as Client
import Vira.Effect.GitHub
import Vira.Lib.GitHub
import Vira.State.Type (JobResult (..))
import Prelude hiding (Reader, asks)

-- | API type for GitHub webhook events
data Routes mode = Routes
  { _pr :: mode :- GitHubEvent PullRequestEvent :> Post '[JSON] NoContent
  , _push :: mode :- GitHubEvent PushEvent :> Post '[JSON] NoContent
  }
  deriving stock (Generic)

-- | Servant handlers for GitHub webhook events
handlers ::
  GlobalSettings ->
  ViraRuntimeState ->
  WebSettings ->
  AppAuth ->
  Routes AsServer
handlers globalSettings viraRuntimeState webSettings appAuth =
  Routes
    { _pr = runWebhookInServant globalSettings viraRuntimeState webSettings appAuth . prHandler appAuth
    , _push = runWebhookInServant globalSettings viraRuntimeState webSettings appAuth . pushHandler
    }

{- | Handle pull request events

On PR open/synchronize: creates a GitHub Check run, enqueues a Vira job,
and registers a callback to update the check run when the job completes.
-}
prHandler :: AppAuth -> PullRequestEvent -> Eff WebhookStack NoContent
prHandler appAuth event = do
  log Info $ "Received PR event: " <> show (evPullReqAction event)
  case evPullReqAction event of
    PullRequestOpenedAction -> handlePrBuild
    PullRequestReopenedAction -> handlePrBuild
    PullRequestActionOther "synchronize" -> handlePrBuild
    _ -> log Debug "Ignoring non-build PR action"
  pure NoContent
  where
    handlePrBuild :: Eff WebhookStack ()
    handlePrBuild = do
      let ghRepo = evPullReqRepo event
          prPayload = evPullReqPayload event
          prTarget = whPullReqHead prPayload
          instId = InstallationId $ fromMaybe 0 $ evPullReqInstallationId event
          owner = Owner $ getUserName (whRepoOwner ghRepo)
          repo = Repo $ whRepoName ghRepo
          commitSha = whPullReqTargetSha prTarget
          branchRef = whPullReqTargetRef prTarget

      -- Create check run and capture the ID
      checkRunResp <-
        queryGitHub @CheckRunResponse instId $
          createCheckRunE owner repo $
            NewCheckRun
              { name = "Vira CI"
              , headSha = commitSha
              , status = Just InProgress
              }

      let checkRunId = checkRunResp.checkRunId

      log Info $ "Created check run " <> show checkRunId <> " for " <> show commitSha

      -- Enqueue job (matching repo by name convention)
      let viraRepoName = RepoName $ whRepoName ghRepo
          viraBranchName = BranchName branchRef
          viraCommitId = CommitID commitSha

      jobId <- Client.enqueueJob viraRepoName viraBranchName viraCommitId

      log Info $ "Enqueued job #" <> show jobId <> " for check run " <> show checkRunId

      -- Register callback to update check run on job completion
      let callback = mkCheckRunCallback appAuth instId owner repo checkRunId
      Client.registerJobCallback jobId callback

    getUserName = either (fromMaybe "" . whSimplUserLogin) whUserLogin

{- | Create a callback that updates a GitHub check run when a job completes

Runs the GitHub effect in IO by creating a minimal effect stack.
Errors are logged but don't propagate (fire-and-forget).
-}
mkCheckRunCallback ::
  AppAuth ->
  InstallationId ->
  Owner ->
  Repo ->
  CheckRunId ->
  Client.JobCallback
mkCheckRunCallback appAuth instId owner repo checkRunId jobResult = do
  let conclusion = case jobResult of
        JobSuccess -> Success
        JobFailure -> Failure
        JobKilled -> Cancelled

      update =
        UpdateCheckRun
          { status = Completed
          , conclusion = Just conclusion
          }

  result <-
    runEff $
      runErrorNoCallStack @GitHubError $
        runGitHubAsApp appAuth $
          queryGitHub_ instId $
            updateCheckRunE owner repo checkRunId update

  case result of
    Left err -> putStrLn $ "Failed to update check run: " <> show err
    Right () -> putStrLn $ "Updated check run " <> show checkRunId <> " with " <> show conclusion

pushHandler :: PushEvent -> Eff WebhookStack NoContent
pushHandler _ = do
  log Info "Received Push"
  pure NoContent

type WebhookStack = GitHub : Error GitHubError : Error ServerError : Eff.Reader WebSettings : AppStack

{- | Run webhook stack into Servant Handler.

Any GitHubError is logged and swallowed
([webhook responds with a 200 response](https://docs.github.com/en/webhooks/using-webhooks/best-practices-for-using-webhooks#respond-within-10-seconds) regardless).
-}
runWebhookInServant ::
  GlobalSettings ->
  ViraRuntimeState ->
  WebSettings ->
  AppAuth ->
  Eff WebhookStack NoContent ->
  Handler NoContent
runWebhookInServant globalSettings viraRuntimeState webSettings appAuth action =
  Handler
    . ExceptT
    . runApp globalSettings viraRuntimeState
    . Eff.runReader webSettings
    . runErrorNoCallStack @ServerError
    . logAndSwallowGitHubError
    . runGitHubAsApp appAuth
    $ do
      tagCurrentThread "🪝"
      action
  where
    logAndSwallowGitHubError ::
      Eff (Error GitHubError : Error ServerError : Eff.Reader WebSettings : AppStack) NoContent ->
      Eff (Error ServerError : Eff.Reader WebSettings : AppStack) NoContent
    logAndSwallowGitHubError m = do
      result <- runErrorNoCallStack @GitHubError m
      case result of
        Left err -> do
          log Warning $ "GitHub API error: " <> show err
          pure NoContent
        Right a -> pure a

{- | WAI middleware that mounts the GitHub webhook at @\/webhook\/github@
TODO: appropriate doc comment
-}
webhookMiddleware ::
  GlobalSettings ->
  ViraRuntimeState ->
  WebSettings ->
  AppAuth ->
  Middleware
webhookMiddleware globalSettings viraRuntimeState webSettings appAuth = \app req sendResponse ->
  case pathInfo req of
    ("webhook" : "github" : rest) -> do
      let req' = req {pathInfo = rest}
      webhookApp req' sendResponse
    _ -> app req sendResponse
  where
    key = maybe mempty encodeUtf8 webSettings.ghWebhookSecret
    githubKey = GitHubKey $ pure key
    webhookApp =
      genericServeTWithContext
        id
        (handlers globalSettings viraRuntimeState webSettings appAuth)
        (githubKey :. EmptyContext)
