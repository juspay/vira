{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infinitely" #-}

module Vira.Refresh.Daemon (
  mkRefreshConfig,
  mkRefreshState,
  requestRefreshRepo,
  startRefreshDaemon,
  runRefreshDaemon,
) where

import Colog.Message (Message)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, putTMVar, readTMVar, readTVar, readTVarIO, retry)
import Data.Map qualified as Map
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Concurrent.Async (Concurrent, async)
import Effectful.Error.Static (
  Error,
  runErrorNoCallStack,
 )
import Effectful.Git (BranchName)
import Effectful.Git qualified as Git
import Effectful.Git.Mirror qualified as Mirror
import Effectful.Process (Process)
import Effectful.Reader.Dynamic (Reader, ask, asks)
import Vira.App qualified as App
import Vira.App.CLI (GlobalSettings)
import Vira.App.Stack (AppState (..))
import Vira.CI.Workspace qualified as Workspace
import Vira.Lib.Logging (Severity (..), log, tagCurrentThread)
import Vira.Refresh.Type (RefreshConfig (..), RefreshDaemon (..), RefreshPriority (..), RefreshState (..), RefreshStatus (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (Repo (..))
import Prelude hiding (Reader, ask, asks, atomically, newTVarIO, putTMVar, readTMVar, readTVar, readTVarIO, writeTVar)

-- | Create a RefreshConfig TVar
mkRefreshConfig :: Maybe Int -> IO (TVar RefreshConfig)
mkRefreshConfig mRefreshInterval = do
  let defaultInterval = 300
      configuredInterval = fromMaybe defaultInterval mRefreshInterval
  newTVarIO $ RefreshConfig (fromIntegral configuredInterval) (configuredInterval > 0)

-- | Create a RefreshState
mkRefreshState :: TVar RefreshConfig -> IO RefreshState
mkRefreshState configTVar =
  RefreshState configTVar
    <$> newTVarIO Map.empty

-- | Get the current RefreshDaemon from AppState
getRefreshDaemon ::
  ( Reader AppState :> es
  , IOE :> es
  ) =>
  Eff es RefreshDaemon
getRefreshDaemon = do
  refreshDaemonTVar <- asks (.refreshDaemon)
  liftIO $ atomically $ readTMVar refreshDaemonTVar

-- | Request a refresh for a specific repository with given priority
requestRefreshRepo ::
  ( Reader AppState :> es
  , IOE :> es
  ) =>
  Git.RepoName ->
  RefreshPriority ->
  Eff es ()
requestRefreshRepo repoName priority = do
  daemon <- getRefreshDaemon
  currentTime <- liftIO getCurrentTime
  liftIO $ atomically $ do
    -- Set status to Pending (or upgrade to Manual if already Pending with Automatic)
    modifyTVar daemon.state.statuses $ \statusMap ->
      case Map.lookup repoName statusMap of
        Just (RefreshPending Automatic _)
          | priority == Manual ->
              -- Upgrade to Manual priority
              Map.insert repoName (RefreshPending Manual currentTime) statusMap
        Just (RefreshPending Manual _) ->
          -- Already Manual, keep existing
          statusMap
        _ ->
          -- Insert new pending status
          Map.insert repoName (RefreshPending priority currentTime) statusMap

-- | Start the refresh daemon background thread and populate the TMVar
startRefreshDaemon ::
  ( Log Message :> es
  , Reader AppState :> es
  , IOE :> es
  ) =>
  GlobalSettings ->
  Maybe Int ->
  Eff es ()
startRefreshDaemon globalSettings mRefreshInterval = do
  appState <- ask

  -- Initialize refresh config and state
  refreshConfig <- liftIO $ mkRefreshConfig mRefreshInterval
  refreshState <- liftIO $ mkRefreshState refreshConfig

  -- Start refresh daemon with app state
  daemonHandle <- liftIO $ Async.async $ App.runApp globalSettings appState runRefreshDaemon

  -- Create daemon with handle and state
  let refreshDaemon = RefreshDaemon daemonHandle refreshState

  -- Update the TMVar with the daemon
  liftIO $ atomically $ putTMVar appState.refreshDaemon refreshDaemon

-- | Run the refresh daemon in the background
runRefreshDaemon ::
  ( Log Message :> es
  , Reader AppState :> es
  , Concurrent :> es
  , IOE :> es
  , Error Text :> es
  , Process :> es
  ) =>
  Eff es ()
runRefreshDaemon = do
  tagCurrentThread "🔄"
  log Info "Refresh daemon started"

  -- Start periodic timer in background
  _ <- async runPeriodicTimer

  -- Main refresh processing loop
  forever $ do
    daemon <- getRefreshDaemon
    repoName <- waitForPendingRepo daemon.state.statuses
    -- Process the refresh
    mRepo <- App.query $ St.GetRepoByNameA repoName
    case mRepo of
      Just repo -> void $ refreshRepository repo
      Nothing -> log Warning $ "Repository not found: " <> repoName.unRepoName
  where
    -- Wait for a pending repo, prioritizing Manual over Automatic
    waitForPendingRepo statusTVar = liftIO $ atomically $ do
      statusMap <- readTVar statusTVar
      let pendingRepos = Map.filter isPending statusMap
      maybe retry pure (findHighestPriority pendingRepos) -- Wait for repos to be added
    isPending (RefreshPending {}) = True
    isPending _ = False

    -- Find repo with highest priority (Manual > Automatic), with deterministic ordering by RepoName
    findHighestPriority :: Map Git.RepoName RefreshStatus -> Maybe Git.RepoName
    findHighestPriority pendingMap =
      let sorted = sortOn (\(name, status) -> (getPriority status, name)) $ Map.toList pendingMap
       in fst <$> viaNonEmpty head sorted

    getPriority :: RefreshStatus -> Int
    getPriority (RefreshPending Manual _) = 0 -- Manual first
    getPriority (RefreshPending Automatic _) = 1 -- Automatic second
    getPriority _ = 2 -- Should not happen

-- | Run the periodic timer that adds all repos to the refresh set
runPeriodicTimer ::
  ( Log Message :> es
  , Reader AppState :> es
  , IOE :> es
  ) =>
  Eff es ()
runPeriodicTimer =
  forever $ do
    daemon <- getRefreshDaemon
    config <- liftIO $ readTVarIO daemon.state.config
    if config.enabled
      then do
        -- Get all repos and mark them as pending with Automatic priority
        allRepos <- App.query St.GetAllReposA
        currentTime <- liftIO getCurrentTime

        liftIO $ atomically $ do
          modifyTVar daemon.state.statuses $ \statusMap ->
            foldr
              ((\repoName acc -> Map.insertWith mergePending repoName (RefreshPending Automatic currentTime) acc) . (.name))
              statusMap
              allRepos

        log Debug $ "Added " <> show (length allRepos) <> " repos to refresh queue"
      else do
        log Debug "Auto-refresh disabled, skipping timer"

    -- Wait for next cycle
    let delayMicroseconds = round (config.refreshInterval * 1_000_000)
    liftIO $ threadDelay delayMicroseconds
  where
    -- When inserting, keep Manual priority if it already exists
    mergePending _new existing@(RefreshPending Manual _) = existing
    mergePending new _ = new

-- | Refresh a single repository
refreshRepository ::
  ( HasCallStack
  , Log Message :> es
  , Reader AppState :> es
  , IOE :> es
  , Error Text :> es
  , Process :> es
  ) =>
  Repo ->
  Eff es (Either Text (Map BranchName Git.Commit))
refreshRepository repo = do
  supervisor <- asks (.supervisor)
  let mirrorPath = Workspace.mirrorPath supervisor repo.name

  log Debug $ "Refreshing repository: " <> repo.name.unRepoName

  result <- runErrorNoCallStack @Text $ do
    -- Sync the mirror
    Mirror.syncMirror repo.cloneUrl mirrorPath
    -- Get remote branches
    Git.remoteBranchesFromClone mirrorPath

  case result of
    Left errorMsg -> do
      log Warning errorMsg
      -- Update refresh status with failure
      currentTime <- liftIO getCurrentTime
      daemon <- getRefreshDaemon
      liftIO $
        atomically $
          modifyTVar daemon.state.statuses $
            Map.insert repo.name (RefreshFailure errorMsg currentTime)
      pure $ Left errorMsg
    Right branches -> do
      -- Update state with new branches
      _ <- App.update $ St.SetRepoBranchesA repo.name branches
      -- Update refresh status with success
      currentTime <- liftIO getCurrentTime
      daemon <- getRefreshDaemon
      liftIO $
        atomically $
          modifyTVar daemon.state.statuses $
            Map.insert repo.name (RefreshSuccess currentTime)
      log Debug $ "Successfully refreshed " <> repo.name.unRepoName <> " (" <> show (Map.size branches) <> " branches)"
      pure $ Right branches
