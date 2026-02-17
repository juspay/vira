{- | GitHub App webhook handler as WAI sub-app

This module provides a standalone WAI application for handling GitHub webhook events.
It is decoupled from the main Vira web server and can be mounted as middleware.
-}
module Vira.Webhook.GitHub (
  webhookMiddleware,
) where

import Colog (Severity (..))
import Effectful (Eff)
import Effectful qualified as Eff
import Effectful.Colog.Simple (log, tagCurrentThread)
import Effectful.Error.Static as Eff (Error, runErrorNoCallStack)
import Effectful.Reader.Dynamic as Eff (Reader, runReader)
import GitHub.Data.Webhooks.Events (
  PullRequestEvent (..),
  PushEvent,
 )
import GitHub.Data.Webhooks.Payload (HookPullRequest (..), HookRepository (..), HookSimpleUser (..), HookUser (..), PullRequestTarget (..))
import Network.Wai (Middleware, pathInfo)
import Servant
import Servant.GitHub.Webhook (GitHubEvent, GitHubKey (..))
import Servant.Server.Generic (AsServer, genericServeTWithContext)
import Vira.App (AppStack, GlobalSettings, ViraRuntimeState, runApp)
import Vira.App.CLI (WebSettings (..))
import Vira.Effect.GitHub
import Vira.Lib.GitHub

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
    { _pr = runWebhookInServant globalSettings viraRuntimeState webSettings appAuth . prHandler
    , _push = runWebhookInServant globalSettings viraRuntimeState webSettings appAuth . pushHandler
    }

-- | Handle pull request events and create a GitHub Check run
prHandler :: PullRequestEvent -> Eff WebhookStack NoContent
prHandler event = do
  log Info $ "Received PR: " <> show event
  createCheckRun event
  pure NoContent
  where
    createCheckRun ::
      (GitHub Eff.:> es) =>
      PullRequestEvent ->
      Eff es ()
    createCheckRun prEvent = do
      let repo = evPullReqRepo prEvent
          prTarget = whPullReqHead $ evPullReqPayload prEvent
          instId = InstallationId $ fromMaybe 0 $ evPullReqInstallationId prEvent

      queryGitHub_
        instId
        ( createCheckRunE
            (Owner $ getUserName (whRepoOwner repo))
            (Repo $ whRepoName repo)
            (mkNewCheckRun prTarget)
        )
      where
        getUserName = either (fromMaybe "" . whSimplUserLogin) whUserLogin
        mkNewCheckRun prTarget =
          NewCheckRun
            { name = "Vira CI"
            , headSha = whPullReqTargetSha prTarget
            , status = Just InProgress
            }

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
TODO: encapsulate github related parameters into one type
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
