{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

{- | GitHub App webhook event handlers

Pure handlers for GitHub webhook events. WAI/middleware concerns are in
"Vira.GitHub.Middleware".

Uses an event-driven approach for check run updates: subscribes to the acid-state
event bus and watches for job status changes, posting GitHub check run updates
as the job transitions through pending, running, and finished states.
-}
module Vira.GitHub.Webhook (
  -- * Servant routes
  Routes (..),
  handlers,

  -- * Webhook helpers
  runWebhookInServant,
  logAndSwallowGitHubError,
) where

import Colog (Severity (..))
import Data.Time (getCurrentTime)
import Effectful (Eff)
import Effectful.Colog.Simple (log, tagCurrentThread)
import Effectful.Concurrent.Async (async)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Error.Static qualified as Error
import Effectful.Git (BranchName (..), Commit (..), CommitID (..), RepoName (..))
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
import Servant
import Servant.GitHub.Webhook (GitHubEvent)
import Servant.Server.Generic (AsServer)
import Vira.App (AppStack, GlobalSettings, ViraRuntimeState, runApp)
import Vira.App.AcidState qualified as App
import Vira.CI.Client qualified as Client
import Vira.Effect.GitHub
import Vira.GitHub.CheckRun qualified as CheckRun
import Vira.Lib.GitHub
import Vira.Refresh qualified as Refresh
import Vira.Refresh.Type (RefreshPriority (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (ForgeInfo (..), OwnerName (..), PRCommit (..), PRState (..), PullRequest (..), prBranchRef)
import Vira.State.Type qualified as St
import Web.TablerIcons.Outline qualified as Icon

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

On PR open/reopen/synchronize: creates PullRequest and PRCommit records.
Same-repo PRs auto-build. Fork PRs require per-commit approval before building.
On PR close/merge: updates PR state.
-}
prHandler :: PullRequestEvent -> Eff (GitHub : Error GitHubError : AppStack) NoContent
prHandler event = do
  log Info $ "Received PR event: " <> show (evPullReqAction event)
  case evPullReqAction event of
    PullRequestOpenedAction -> handlePROpened
    PullRequestReopenedAction -> handlePROpened
    PullRequestActionOther "synchronize" -> handlePROpened
    PullRequestActionOther "closed" -> handlePRClosed
    _ -> log Debug "Ignoring non-build PR action"
  pure NoContent
  where
    handlePROpened :: Eff (GitHub : Error GitHubError : AppStack) ()
    handlePROpened = do
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
              , prState = PROpen
              , ..
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
            PRCommit
              { repo = pr.repo
              , prNumber = pr.prNumber
              , approved = not isFork
              , ..
              }
      App.update $ St.AddPRCommitA prCommit

      -- Store commit in the global index so job rows can display it
      App.update $ St.StoreCommitA commit

      -- Subscribe to event bus BEFORE enqueuing to avoid missing events
      let instId = InstallationId installationId
          owner = Owner $ unOwnerName baseOwner
          ghRepo = Repo $ unRepoName repo
      chan <- App.subscribe

      -- Same-repo PRs: auto-build (fork PRs wait for approval via core route)
      unless isFork $ do
        let branchRef = prBranchRef pr.prNumber
        void $ Client.enqueueJob pr.repo branchRef prCommit.commit.id (Just pr.prNumber)

      -- Spawn check run watcher for all PRs (same-repo + fork)
      -- installationId is captured in the async closure — never persisted
      void $
        async $
          CheckRun.prCheckRunWatcher chan instId owner ghRepo pr.repo pr.prNumber prCommit.commit.id

    handlePRClosed :: Eff (GitHub : Error GitHubError : AppStack) ()
    handlePRClosed = do
      let prRepo = evPullReqRepo event
          repo = RepoName $ whRepoName prRepo
          prPayload = evPullReqPayload event
          prNum = whPullReqNumber prPayload
          newState = if isJust (whPullReqMergedAt prPayload) then PRMerged else PRClosed
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
      let repoNames = toList $ fmap (RepoName . whSimplRepoName) (evInstallationReposRemove event)
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
