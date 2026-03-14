{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub App webhook integration

Handles incoming GitHub webhook events (PR, push, installation) and manages
the check run lifecycle — watching the acid-state event bus for job status
changes and posting updates back to GitHub.

Includes WAI middleware that mounts these routes under @/github/webhook@.
-}
module Vira.Web.Webhook.GitHub (
  -- * WAI middleware
  githubMiddleware,

  -- * Servant routes
  Routes (..),
  handlers,

  -- * Webhook helpers
  runWebhookInServant,
  logAndSwallowGitHubError,

  -- * Check run lifecycle
  pullRequestCheckRunWatcher,
  jobStatusLoop,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Control.Concurrent.STM (TChan)
import Data.Acid.Events (SomeUpdate)
import Data.Acid.Events qualified as Events
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE)
import Effectful qualified as E
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), log, tagCurrentThread)
import Effectful.Concurrent.Async (async)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Error.Static qualified as Error
import Effectful.Git (BranchName (..), Commit (..), CommitID (..), RepoName (..))
import Effectful.Reader.Static qualified as ER
import GitHub.Data.Webhooks.Events (
  InstallationEvent (..),
  InstallationEventAction (..),
  InstallationRepoEventAction (..),
  InstallationRepositoriesEvent (..),
  PullRequestEvent (..),
  PullRequestEventAction (..),
  PushEvent,
 )
import GitHub.Data.Webhooks.Payload (HookPullRequest (..), HookRepository (..), HookRepositorySimple (..), HookUser (..), PullRequestTarget (..), URL (..))
import Network.Wai (Middleware, pathInfo)
import Servant
import Servant.GitHub.Webhook (GitHubEvent, GitHubKey (..))
import Servant.Server.Generic (AsServer, genericServeTWithContext)
import Vira.App (AppStack, GlobalSettings, ViraRuntimeState, runApp)
import Vira.App.AcidState qualified as App
import Vira.CI.Client qualified as Client
import Vira.Effect.GitHub
import Vira.Lib.GitHub
import Vira.Refresh qualified as Refresh
import Vira.Refresh.Type (RefreshPriority (..))
import Vira.State.Acid (AddNewJobA (..), JobUpdateStatusA (..))
import Vira.State.Acid qualified as St
import Vira.State.Core (ViraState)
import Vira.State.Type (ForgeInfo (..), Job (..), JobId, JobResult (..), JobStatus (..), OwnerName (..), PullRequest (..), PullRequestCommit (..), PullRequestState (..), pullRequestBranchRef)
import Vira.State.Type qualified as St
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (Reader)

-- * WAI Middleware

-- | WAI middleware that mounts GitHub webhook routes under @/github/webhook@
githubMiddleware ::
  GlobalSettings ->
  ViraRuntimeState ->
  AppAuth ->
  Text ->
  Middleware
githubMiddleware globalSettings viraRuntimeState appAuth webhookSecret app req sendResponse =
  case pathInfo req of
    ("github" : "webhook" : rest) -> do
      let req' = req {pathInfo = rest}
      webhookApp req' sendResponse
    _ -> app req sendResponse
  where
    key = encodeUtf8 webhookSecret
    githubKey = GitHubKey $ pure key
    webhookApp =
      genericServeTWithContext
        Prelude.id
        (handlers globalSettings viraRuntimeState appAuth)
        (githubKey :. EmptyContext)

-- * Servant Routes

-- | API type for GitHub webhook events
data Routes mode = Routes
  { _pullRequest :: mode :- GitHubEvent PullRequestEvent :> Post '[JSON] NoContent
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
    { _pullRequest =
        runWebhookInServant globalSettings viraRuntimeState
          . logAndSwallowGitHubError appAuth
          . pullRequestHandler
    , _push = runWebhookInServant globalSettings viraRuntimeState . pushHandler
    , _installation = runWebhookInServant globalSettings viraRuntimeState . installationHandler
    , _installationRepos = runWebhookInServant globalSettings viraRuntimeState . installationReposHandler
    }

{- | Handle pull request events

On PR open/reopen/synchronize: creates PullRequest and PullRequestCommit records.
Same-repo PRs auto-build. Fork PRs require per-commit approval before building.
On PR close/merge: updates PR state.
-}
pullRequestHandler :: PullRequestEvent -> Eff (GitHub : Error GitHubError : AppStack) NoContent
pullRequestHandler event = do
  log Info $ "Received PR event: " <> show (evPullReqAction event)
  case evPullReqAction event of
    PullRequestOpenedAction -> handlePullRequestOpened
    PullRequestReopenedAction -> handlePullRequestOpened
    PullRequestActionOther "synchronize" -> handlePullRequestOpened
    PullRequestActionOther "closed" -> handlePullRequestClosed
    _ -> log Debug "Ignoring non-build PR action"
  pure NoContent
  where
    handlePullRequestOpened :: Eff (GitHub : Error GitHubError : AppStack) ()
    handlePullRequestOpened = do
      let prRepo = evPullReqRepo event
          prPayload = evPullReqPayload event
          prHead = whPullReqHead prPayload
          prBase = whPullReqBase prPayload
          repo = RepoName $ whRepoName prRepo
          headOwner = OwnerName $ whUserLogin $ whPullReqTargetUser prHead
          baseOwner = OwnerName $ whUserLogin $ whPullReqTargetUser prBase
          isFork = headOwner /= baseOwner
          URL htmlUrl = whPullReqHtmlUrl prPayload
          forgeInfo = Just $ ForgeInfo htmlUrl Icon.brand_github

      installationId <- case evPullReqInstallationId event of
        Just i -> pure i
        Nothing -> do
          log Error "PR event missing installation ID"
          Error.throwError $ TokenFetchFailed "Missing installation ID in PR event"

      let pr =
            PullRequest
              { prNumber = whPullReqNumber prPayload
              , title = whPullReqTitle prPayload
              , headBranch = BranchName $ whPullReqTargetRef prHead
              , baseBranch = BranchName $ whPullReqTargetRef prBase
              , prState = PullRequestOpen
              , repo
              , headOwner
              , baseOwner
              , forgeInfo
              }
      App.update $ St.UpsertPullRequestA pr

      now <- liftIO getCurrentTime
      let commit =
            Commit
              { id = CommitID $ whPullReqTargetSha prHead
              , message = whPullReqTitle prPayload
              , -- Cannot be fetched from a prPayload
                date = now
              , author = ""
              , authorEmail = ""
              }
          prCommit =
            PullRequestCommit
              { repo = pr.repo
              , prNumber = pr.prNumber
              , approved = not isFork
              , commit
              }
      App.update $ St.AddPullRequestCommitA prCommit

      -- Store commit in the global index so job rows can display it
      App.update $ St.StoreCommitA commit

      -- Subscribe to event bus BEFORE enqueuing to avoid missing events
      let instId = InstallationId installationId
          owner = Owner $ unOwnerName baseOwner
          ghRepo = Repo $ unRepoName repo
      chan <- App.subscribe

      -- Same-repo PRs: auto-build (fork PRs wait for approval via core route)
      unless isFork $ do
        let branchRef = pullRequestBranchRef pr.prNumber
        void $ Client.enqueueJob pr.repo branchRef prCommit.commit.id (Just pr.prNumber)

      -- Spawn check run watcher for all PRs (same-repo + fork)
      -- installationId is captured in the async closure — never persisted
      void $
        async $
          pullRequestCheckRunWatcher chan instId owner ghRepo pr.repo pr.prNumber prCommit.commit.id

    handlePullRequestClosed :: Eff (GitHub : Error GitHubError : AppStack) ()
    handlePullRequestClosed = do
      let prRepo = evPullReqRepo event
          repo = RepoName $ whRepoName prRepo
          prPayload = evPullReqPayload event
          prNum = whPullReqNumber prPayload
          newState = if isJust (whPullReqMergedAt prPayload) then PullRequestMerged else PullRequestClosed
      App.update $ St.UpdatePullRequestStateA repo prNum newState
      log Info $ "PR " <> show prNum <> " " <> show newState

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
      let repoNames = toList $ fmap (RepoName . whSimplRepoName) (evInstallationRepos event)
      log Info "Installation deleted, removing repositories from Vira"
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
      let repoNames = toList $ fmap (RepoName . whSimplRepoName) (evInstallationReposRemove event)
      log Info "Repositories removed from installation, deleting from Vira"
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
        repoName = RepoName $ whSimplRepoName repoSimple
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

-- * Webhook helpers

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
      log Error $ "GitHub API error: " <> show err
      pure NoContent
    Right a -> pure a

-- * Check run lifecycle

{- | Unified PR check run watcher

Spawned by the webhook handler for every PR event (open/reopen/synchronize).
Waits for a job matching this PR + commit, then creates a GitHub check run
and watches for status updates until the job finishes.

For same-repo PRs: the job is enqueued before this watcher starts, so the
matching 'AddNewJobA' event is found immediately.
For fork PRs: this watcher blocks until the core approval route enqueues the job.

The @installationId@ is captured in the async closure — never persisted in core state.
-}
pullRequestCheckRunWatcher ::
  ( (E.:>) GitHub es
  , (E.:>) (ER.Reader LogContext) es
  , (E.:>) (Log (RichMessage IO)) es
  , (E.:>) IOE es
  ) =>
  TChan (SomeUpdate ViraState) ->
  InstallationId ->
  Owner ->
  Repo ->
  RepoName ->
  Int ->
  CommitID ->
  Eff es ()
pullRequestCheckRunWatcher chan instId owner repo repoName prNum commitId = do
  -- Wait for a job matching this PR + commit
  log Info $ "Watching for job on PR #" <> show prNum <> " commit " <> show commitId
  jobId <- liftIO $ waitForPullRequestJob chan repoName prNum commitId
  log Info $ "Found job " <> show jobId <> " for PR #" <> show prNum

  -- Create check run and watch status
  checkRun <-
    queryGitHub @CheckRun instId $
      createCheckRunE owner repo $
        NewCheckRun
          { name = "Vira CI"
          , headSha = unCommitID commitId
          , status = Just Queued
          }
  log Info $ "Created check run for PR #" <> show prNum <> " commit " <> show commitId
  jobStatusLoop chan instId owner repo checkRun.checkRunId jobId

-- | Wait for an 'AddNewJobA' event matching the given PR and commit
waitForPullRequestJob :: TChan (SomeUpdate ViraState) -> RepoName -> Int -> CommitID -> IO JobId
waitForPullRequestJob chan repoName prNum commitId = do
  updates <- Events.awaitBatched chan matchesPRJob 500_000
  -- The last matching event has the job we want
  let extractJobId u = case Events.matchUpdate @AddNewJobA u of
        Just (AddNewJobA r _ c (Just n) _ _, job) | r == repoName && n == prNum && c == commitId -> Just job.jobId
        _ -> Nothing
  case mapMaybe extractJobId (toList updates) of
    (jid : _) -> pure jid
    [] -> waitForPullRequestJob chan repoName prNum commitId -- shouldn't happen, but retry
  where
    matchesPRJob update =
      case Events.matchUpdate @AddNewJobA update of
        Just (AddNewJobA r _ c (Just n) _ _, _) -> r == repoName && n == prNum && c == commitId
        _ -> False

-- | Watch event bus for job status changes, updating the GitHub check run
jobStatusLoop ::
  ( (E.:>) GitHub es
  , (E.:>) IOE es
  , (E.:>) (Log (RichMessage IO)) es
  , (E.:>) (ER.Reader LogContext) es
  ) =>
  TChan (SomeUpdate ViraState) ->
  InstallationId ->
  Owner ->
  Repo ->
  CheckRunId ->
  JobId ->
  Eff es ()
jobStatusLoop chan instId owner repo checkRunId jobId = do
  updates <- liftIO $ Events.awaitBatched chan (matchesJob jobId) 500_000
  let latestStatus = lastStatus updates
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

-- | Convert a 'JobStatus' to a GitHub check run update
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
