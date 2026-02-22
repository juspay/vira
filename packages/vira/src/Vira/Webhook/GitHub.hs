{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub App webhook handler as WAI sub-app

This module provides a standalone WAI application for handling GitHub webhook events.
It is decoupled from the main Vira web server and can be mounted as middleware.

Uses an event-driven approach for check run updates: subscribes to the acid-state
event bus and watches for job status changes, posting GitHub check run updates
as the job transitions through pending, running, and finished states.
-}
module Vira.Webhook.GitHub (
  webhookMiddleware,
) where

import Colog (Severity (..))
import Control.Concurrent.STM (TChan)
import Data.Acid.Events (SomeUpdate)
import Data.Acid.Events qualified as Events
import Effectful (Eff)
import Effectful.Colog.Simple (log, tagCurrentThread)
import Effectful.Concurrent.Async (async)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Git (BranchName (..), CommitID (..), RepoName (..))
import GitHub.Data.Webhooks.Events (
  InstallationEvent (..),
  InstallationEventAction (..),
  InstallationRepoEventAction (..),
  InstallationRepositoriesEvent (..),
  PullRequestEvent (..),
  PullRequestEventAction (..),
  PushEvent,
 )
import GitHub.Data.Webhooks.Payload (HookPullRequest (..), HookRepository (..), HookRepositorySimple (..), PullRequestTarget (..))
import Network.Wai (Middleware, pathInfo)
import Servant
import Servant.GitHub.Webhook (GitHubEvent, GitHubKey (..))
import Servant.Server.Generic (AsServer, genericServeTWithContext)
import Vira.App (AppStack, GlobalSettings, ViraRuntimeState (..), runApp)
import Vira.App.AcidState qualified as App
import Vira.App.CLI (WebSettings (..))
import Vira.CI.Client qualified as Client
import Vira.Effect.GitHub
import Vira.Lib.GitHub
import Vira.Refresh qualified as Refresh
import Vira.Refresh.Type (RefreshPriority (..))
import Vira.State.Acid (JobUpdateStatusA (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (Job (..), JobId, JobResult (..), JobStatus (..), ViraState)
import Vira.State.Type qualified as St

-- | API type for GitHub webhook events
data Routes mode = Routes
  { _pr :: mode :- GitHubEvent PullRequestEvent :> Post '[JSON] NoContent
  , _push :: mode :- GitHubEvent PushEvent :> Post '[JSON] NoContent
  , _installation :: mode :- GitHubEvent InstallationEvent :> Post '[JSON] NoContent
  , _installationRepos :: mode :- GitHubEvent InstallationRepositoriesEvent :> Post '[JSON] NoContent
  }
  deriving stock (Generic)

-- | Servant handlers for GitHub webhook events
handlers ::
  GlobalSettings ->
  ViraRuntimeState ->
  AppAuth ->
  Routes AsServer
handlers globalSettings viraRuntimeState appAuth =
  Routes
    { _pr =
        runWebhookInServant globalSettings viraRuntimeState
          . logAndSwallowGitHubError appAuth
          . prHandler
    , _push = runWebhookInServant globalSettings viraRuntimeState . pushHandler
    , _installation = runWebhookInServant globalSettings viraRuntimeState . installationHandler
    , _installationRepos = runWebhookInServant globalSettings viraRuntimeState . installationReposHandler
    }

{- | Handle pull request events

On PR open/synchronize: subscribes to the event bus, creates a GitHub check run
with Queued status, enqueues a Vira job, and spawns an async watcher that updates
the check run as the job progresses through pending, running, and finished states.
-}
prHandler :: PullRequestEvent -> Eff (GitHub : Error GitHubError : AppStack) NoContent
prHandler event = do
  log Info $ "Received PR event: " <> show (evPullReqAction event)
  case evPullReqAction event of
    PullRequestOpenedAction -> handlePrBuild
    PullRequestReopenedAction -> handlePrBuild
    PullRequestActionOther "synchronize" -> handlePrBuild
    _ -> log Debug "Ignoring non-build PR action"
  pure NoContent
  where
    handlePrBuild :: Eff (GitHub : Error GitHubError : AppStack) ()
    handlePrBuild = do
      let prRepo = evPullReqRepo event
          prHead = whPullReqHead $ evPullReqPayload event
          instId = InstallationId $ fromMaybe 0 $ evPullReqInstallationId event
          owner = hookUserLoginAny (whRepoOwner prRepo)
          repo = whRepoName prRepo
          commit = whPullReqTargetSha prHead

      -- Subscribe to event bus BEFORE enqueueing to avoid missing events
      chan <- App.subscribe

      -- Create check run with Queued status
      checkRunResp <-
        queryGitHub @CheckRunResponse instId $
          createCheckRunE (Owner owner) (Repo repo) $
            NewCheckRun
              { name = "Vira CI"
              , headSha = commit
              , status = Just Queued
              }

      let checkRunId = checkRunResp.checkRunId
      log Info $ "Created check run for " <> show commit

      -- Enqueue job
      job <-
        Client.enqueueJob
          (RepoName $ owner <> "/" <> repo)
          (BranchName $ whPullReqTargetRef prHead)
          (CommitID commit)

      -- Spawn async watcher to update check run as job progresses
      void $ async $ jobStatusLoop chan instId (Owner owner) (Repo repo) checkRunId job.jobId

-- \| Watch event bus for job status changes, updating the GitHub check run accordingly
jobStatusLoop ::
  TChan (SomeUpdate ViraState) ->
  InstallationId ->
  Owner ->
  Repo ->
  CheckRunId ->
  JobId ->
  Eff (GitHub : Error GitHubError : AppStack) ()
jobStatusLoop chan instId owner repo checkRunId jobId = do
  updates <- liftIO $ Events.awaitBatched chan (matchesJob jobId) 500_000
  let latestStatus = lastStatus updates
  -- Catch errors within loop to avoid killing the async task
  -- TODO: verify the LLM's claim
  result <-
    runErrorNoCallStack @GitHubError $
      queryGitHub_ instId $
        updateCheckRunE owner repo checkRunId $
          fromJobStatus latestStatus
  case result of
    Left err -> log Warning $ "Failed to update check run " <> show checkRunId <> ": " <> show err
    Right () -> pass
  unless (isTerminal latestStatus) $ jobStatusLoop chan instId owner repo checkRunId jobId
  where
    matchesJob targetJobId update =
      case Events.matchUpdate @JobUpdateStatusA update of
        Just (JobUpdateStatusA jid _, _) -> jid == targetJobId
        Nothing -> False

    lastStatus updates =
      let extractStatus u = case Events.matchUpdate @JobUpdateStatusA u of
            Just (JobUpdateStatusA _ s, _) -> Just s
            Nothing -> Nothing
       in fromMaybe JobPending $ viaNonEmpty last $ mapMaybe extractStatus (toList updates)

    isTerminal = \case
      JobFinished {} -> True
      JobStale -> True
      _ -> False

pushHandler :: PushEvent -> Eff AppStack NoContent
pushHandler _ = do
  log Info "Received Push"
  pure NoContent

{- | Handle installation events (created/deleted)

On installation created: automatically add all repositories to Vira registry
On installation deleted: remove all repositories from Vira (unless they have running jobs)
-}
installationHandler :: InstallationEvent -> Eff AppStack NoContent
installationHandler event = do
  log Info $ "Received installation event: " <> show (evInstallationAction event)
  case evInstallationAction event of
    InstallationCreatedAction -> do
      let repos = toList $ evInstallationRepos event
      addRepositories repos
    InstallationDeletedAction -> do
      let repoNames = toList $ fmap (RepoName . whSimplRepoFullName) (evInstallationRepos event)
      log Warning "Installation deleted, removing repositories from Vira"
      deleteRepositories repoNames
    _ -> log Debug "Ignoring non-create/delete installation action"
  pure NoContent

{- | Handle installation_repositories events (added/removed)

On repositories added: automatically add them to Vira registry
On repositories removed: remove them from Vira (unless they have running jobs)
-}
installationReposHandler :: InstallationRepositoriesEvent -> Eff AppStack NoContent
installationReposHandler event = do
  log Info $ "Received installation_repositories event: " <> show (evInstallationRepoAction event)
  case evInstallationRepoAction event of
    InstallationRepoAddedAction -> do
      let repos = toList $ evInstallationReposAdd event
      addRepositories repos
    InstallationRepoRemovedAction -> do
      let repoNames = toList $ fmap (RepoName . whSimplRepoFullName) (evInstallationReposRemove event)
      log Warning "Repositories removed from installation, deleting from Vira"
      deleteRepositories repoNames
    _ -> log Debug "Ignoring unknown installation_repositories action"
  pure NoContent

{- | Add repositories from webhook event

For each repository:
1. Check if it already exists (idempotent)
2. Add to Vira registry with constructed clone URL
3. Schedule immediate refresh to fetch branches
-}
addRepositories :: [HookRepositorySimple] -> Eff AppStack ()
addRepositories repos = do
  forM_ repos $ \repoSimple -> do
    let fullName = whSimplRepoFullName repoSimple
        repoName = RepoName fullName
        cloneUrl = "https://github.com/" <> fullName <> ".git"

    log Info $ "Processing repository: " <> fullName

    -- Check for duplicates
    App.query (St.GetRepoByNameA repoName) >>= \case
      Just _ -> log Info $ "Repository already exists, skipping: " <> toText repoName
      Nothing -> do
        -- Add repository
        let newRepo =
              St.Repo
                { name = repoName
                , cloneUrl = cloneUrl
                , lastRefresh = Nothing
                }
        App.update $ St.AddNewRepoA newRepo
        log Info $ "Added repository: " <> toText repoName

        -- Schedule immediate refresh (matches pattern from RegistryPage.hs)
        Refresh.scheduleRepoRefresh (one repoName) Now

{- | Delete repositories from Vira

For each repository:
1. Attempt deletion (fails if running jobs exist)
2. Log success or failure
-}
deleteRepositories :: [RepoName] -> Eff AppStack ()
deleteRepositories repoNames = do
  forM_ repoNames $ \repoName -> do
    log Info $ "Deleting repository: " <> toText repoName
    App.update (St.DeleteRepoByNameA repoName) >>= \case
      Left errMsg -> log Error $ "Failed to delete " <> toText repoName <> ": " <> errMsg
      Right () -> log Info $ "Deleted repository: " <> toText repoName

-- | Run an 'AppStack' action into Servant 'Handler'
runWebhookInServant :: GlobalSettings -> ViraRuntimeState -> Eff AppStack NoContent -> Handler NoContent
runWebhookInServant globalSettings viraRuntimeState action =
  Handler
    . ExceptT
    . fmap Right
    . runApp globalSettings viraRuntimeState
    $ do
      tagCurrentThread "🪝"
      action

{- | Interpret @GitHub : Error GitHubError@ down to 'AppStack'

Any 'GitHubError' is logged and swallowed
([webhook responds with a 200 response](https://docs.github.com/en/webhooks/using-webhooks/best-practices-for-using-webhooks#respond-within-10-seconds) regardless).
-}
logAndSwallowGitHubError :: AppAuth -> Eff (GitHub : Error GitHubError : AppStack) NoContent -> Eff AppStack NoContent
logAndSwallowGitHubError appAuth m = do
  result <- runErrorNoCallStack @GitHubError $ runGitHubAsApp appAuth m
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
        (handlers globalSettings viraRuntimeState appAuth)
        (githubKey :. EmptyContext)

-- Helpers
fromJobStatus :: JobStatus -> UpdateCheckRun
fromJobStatus = \case
  JobRunning ->
    UpdateCheckRun {status = InProgress, conclusion = Nothing}
  JobFinished jobResult _ -> do
    let conclusion = case jobResult of
          JobSuccess -> Success
          JobFailure -> Failure
          JobKilled -> Cancelled
    UpdateCheckRun {status = Completed, conclusion = Just conclusion}
  JobStale ->
    UpdateCheckRun {status = Completed, conclusion = Just Cancelled}
  JobPending -> UpdateCheckRun {status = Queued, conclusion = Nothing}
