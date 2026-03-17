{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | GitHub App webhook integration

Handles incoming GitHub webhook events (PR, push, installation) and manages
the check run lifecycle — watching the acid-state event bus for job status
changes and posting updates back to GitHub.

Includes WAI middleware that mounts these routes under @/github/webhook@.
-}
module Vira.Web.Webhook.GitHub (
  githubMiddleware,
)
where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Control.Concurrent.STM qualified as STM
import Data.Acid.Events qualified as Events
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE)
import Effectful qualified as E
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..), log, tagCurrentThread, withLogContext)
import Effectful.Concurrent.Async (async)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Error.Static qualified as Error
import Effectful.Git (CommitID (..), RepoName (..))
import Effectful.Reader.Static qualified as ER
import GitHub.Data.Webhooks.Events (
  InstallationEvent (..),
  InstallationEventAction (..),
  PullRequestEvent (..),
  PullRequestEventAction (..),
  PushEvent,
 )
import GitHub.Data.Webhooks.Payload (HookPullRequest (..), HookRepository (..), HookRepositorySimple (..))
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
import Vira.State.Type (Job (..), JobStatus (..), PullRequest (..), PullRequestCommit (..), PullRequestState (..), pullRequestBranchRef)
import Vira.State.Type qualified as St
import Prelude hiding (Reader)

data Routes mode = Routes
  { _pullRequest :: mode :- GitHubEvent PullRequestEvent :> Post '[JSON] NoContent
  , _push :: mode :- GitHubEvent PushEvent :> Post '[JSON] NoContent
  , _installation :: mode :- GitHubEvent InstallationEvent :> Post '[JSON] NoContent
  }
  deriving stock (Generic)

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
    }

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
      App.update $ St.AddPullRequestA pr

      now <- liftIO getCurrentTime -- FIXME: not a true indicator of when the commit was made, but the payload doesn't provide one
      let (prCommit, commit) = toPullRequestCommit now event
      App.update $ St.AddPullRequestCommitA prCommit commit

      installationId <- getInstallationId
      chan <- App.subscribe

      unless isFork $
        void $
          Client.enqueueJob pr.repo branchRef prCommit.commitId (Just pr.prNumber)

      void $
        async $
          checkRunLoop chan (InstallationId installationId) pr prCommit.commitId
      where
        pr = toPullRequest event
        isFork = pr.headOwner /= pr.baseOwner
        branchRef = pullRequestBranchRef pr.prNumber
        getInstallationId =
          case evPullReqInstallationId event of
            Just i -> pure i
            Nothing -> do
              log Error "PR event missing installation ID"
              Error.throwError $ TokenFetchFailed "Missing installation ID in PR event"

    handlePullRequestClosed :: Eff (GitHub : Error GitHubError : AppStack) ()
    handlePullRequestClosed = do
      let prRepo = evPullReqRepo event
          repo = RepoName $ whRepoName prRepo
          prPayload = evPullReqPayload event
          prNum = whPullReqNumber prPayload
          newState = if isJust (whPullReqMergedAt prPayload) then PullRequestMerged else PullRequestClosed
      App.update $ St.SetPullRequestStateA repo prNum newState
      log Info $ "PR " <> show prNum <> " " <> show newState

pushHandler :: PushEvent -> Eff AppStack NoContent
pushHandler _ = do
  -- TODO: Handle
  log Info "Received Push"
  pure NoContent

{- | Check run event loop

For same-repo PRs: the job is enqueued before this loop starts, so the
matching 'AddNewJobA' event is found immediately.
For fork PRs: this loop blocks until the core approval route enqueues the job.
-}
checkRunLoop ::
  ( (E.:>) GitHub es
  , (E.:>) (ER.Reader LogContext) es
  , (E.:>) (Log (RichMessage IO)) es
  , (E.:>) IOE es
  ) =>
  STM.TChan (Events.SomeUpdate ViraState) ->
  InstallationId ->
  PullRequest ->
  CommitID ->
  Eff es ()
checkRunLoop chan instId pr commitId = do
  log Info $ "Watching for job on PR " <> show pr.prNumber <> " commit " <> show commitId

  jobId <- liftIO awaitJob
  log Info $ "Found job " <> show jobId <> " for PR " <> show pr.prNumber

  checkRun <-
    queryGitHub @CheckRun instId $
      createCheckRunE pr.baseOwner pr.repo $
        NewCheckRun "Vira CI" (unCommitID commitId) (Just Queued)
  log Info $ "Created check run for PR " <> show pr.prNumber <> " commit " <> show commitId

  fix $ \loop -> do
    status <- liftIO $ awaitStatusChange jobId
    updateCheckRunStatus checkRun.checkRunId status
    unless (isTerminal status) loop
  where
    awaitJob =
      awaitMatch chan $ \u ->
        case Events.matchUpdate @AddNewJobA u of
          Just (AddNewJobA r _ c (Just n) _ _, job) | r == pr.repo && n == pr.prNumber && c == commitId -> Just job.jobId
          _ -> Nothing

    awaitStatusChange targetJobId =
      awaitMatch chan $ \u ->
        case Events.matchUpdate @JobUpdateStatusA u of
          Just (JobUpdateStatusA jid s, _) | jid == targetJobId -> Just s
          _ -> Nothing

    updateCheckRunStatus checkRunId status = do
      result <-
        runErrorNoCallStack @GitHubError $
          queryGitHub_ instId $
            updateCheckRunE pr.baseOwner pr.repo checkRunId $
              fromJobStatus status
      case result of
        Left err -> log Warning $ "Failed to update check run " <> show checkRunId <> ": " <> show err
        Right () -> pass

    isTerminal = \case
      JobFinished {} -> True
      JobStale -> True
      _ -> False

installationHandler :: InstallationEvent -> Eff AppStack NoContent
installationHandler event = do
  log Info $ "Received installation event: " <> show (evInstallationAction event)
  case evInstallationAction event of
    InstallationCreatedAction -> do
      let repos = toList $ evInstallationRepos event
      addRepositories repos
    _ -> log Debug "Ignoring non-create installation action"
  pure NoContent
  where
    addRepositories :: [HookRepositorySimple] -> Eff AppStack ()
    addRepositories repos = do
      forM_ repos $ \repoSimple -> do
        let fullName = whSimplRepoFullName repoSimple
            repoName = RepoName $ whSimplRepoName repoSimple
            cloneUrl = "https://github.com/" <> fullName <> ".git" -- LIMITATION: won't work with private repos, but `HookRepositorySimple` has no field for url
        App.query (St.GetRepoByNameA repoName) >>= \case
          Just _ -> log Info $ "Repository already exists, skipping: " <> toText repoName
          Nothing -> do
            App.update $ St.AddNewRepoA $ St.Repo repoName cloneUrl Nothing
            log Info $ "Added repository: " <> toText repoName
            Refresh.scheduleRepoRefresh (one repoName) Now

-- * Webhook helpers

runWebhookInServant :: GlobalSettings -> ViraRuntimeState -> Eff AppStack NoContent -> Handler NoContent
runWebhookInServant globalSettings viraRuntimeState action =
  Handler
    . ExceptT
    . fmap Right
    . runApp globalSettings viraRuntimeState
    $ do
      tagCurrentThread "🪝"
      withLogContext [("webhook", "github")] action

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

-- | Read events one at a time, returning the first successful extraction
awaitMatch :: STM.TChan (Events.SomeUpdate ViraState) -> (Events.SomeUpdate ViraState -> Maybe a) -> IO a
awaitMatch chan extract = do
  u <- STM.atomically $ STM.readTChan chan
  maybe (awaitMatch chan extract) pure (extract u)

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
