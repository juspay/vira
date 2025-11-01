{-# LANGUAGE OverloadedRecordDot #-}

-- | Refresh daemon for automatic repository updates
module Vira.Refresh.Daemon (
  startRefreshDaemon,
) where

import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TChan (readTChan)
import Data.Acid.Events qualified as Events
import Data.Map.Strict qualified as Map
import Data.Time (diffUTCTime, getCurrentTime)
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog.Simple (Severity (..), log, tagCurrentThread, withLogContext)
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async (Concurrent, async)
import Effectful.Concurrent.STM (atomically)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Git (BranchName, Commit, RepoName (..))
import Effectful.Git.Command.ForEachRef qualified as Git
import Effectful.Git.Mirror qualified as Mirror
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.AcidState qualified as App
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.CI.Workspace qualified as Workspace
import Vira.Lib.TimeExtra (formatDuration)
import Vira.Refresh.Core (initializeRefreshState, scheduleRepoRefresh)
import Vira.Refresh.Type (RefreshOutcome (..), RefreshPriority (..), RefreshResult (..), RefreshState (..), RefreshStatus (..))
import Vira.State.Acid (DeleteRepoByNameA (..), GetAllReposA (..), GetRepoByNameA (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (Branch (..), Repo (..))
import Prelude hiding (Reader, asks, atomically)

-- | Start the refresh daemon
startRefreshDaemon :: Eff AppStack ()
startRefreshDaemon = do
  log Info "🔄 Refresh daemon starting..."

  -- Initialize refresh state from persisted data
  initializeRefreshState
  log Info "🔄 Loaded refresh status from acid-state"

  void $ async schedulerLoop
  void $ async cleanupWorker
  workerHandle <- async workerLoop

  st <- asks (.refreshState)
  atomically $ writeTVar st.daemonHandle (Just workerHandle)

  log Info "🔄 Refresh daemon started (scheduler + cleanup + worker)"

-- | Scheduler loop: periodically set all repos to Pending with Normal priority
schedulerLoop :: Eff AppStack Void
schedulerLoop = do
  tagCurrentThread "🔄"
  infinitely $ do
    repos <- App.query GetAllReposA
    log Info $ "Scheduling refresh for " <> show (length repos) <> " repos"
    forM_ repos $ \repo ->
      withLogContext [("repo", show repo.name)] $ do
        scheduleRepoRefresh repo.name Normal
    threadDelay (5 * 60 * 1000000) -- 5 minutes in microseconds

-- | Cleanup worker: subscribe to repo deletion events and clean up refresh state
cleanupWorker :: Eff AppStack Void
cleanupWorker = do
  tagCurrentThread "🔄"
  chan <- App.subscribe

  infinitely $ do
    someUpdate <- atomically $ readTChan chan
    whenJust (Events.matchUpdate someUpdate) $ \(DeleteRepoByNameA name, _) -> do
      log Info $ "Repo deleted, cleaning up refresh state: " <> show name
      removeRepoFromRefreshState name

-- | Worker loop: continuously process pending repos
workerLoop :: Eff AppStack Void
workerLoop = do
  tagCurrentThread "🔄"
  infinitely $ do
    -- Pop next pending repo (blocks via STM retry until available)
    repoName <- popNextPendingRepo

    -- Fetch repo data and refresh
    App.query (GetRepoByNameA repoName) >>= \case
      Nothing -> do
        -- Should not happen - cleanup worker should remove deleted repos
        log Error $ "Repo not found in state (cleanup worker should have removed it): " <> show repoName
      Just repo -> refreshRepo repo

{- | Atomically pop the next pending repo and mark it as InProgress
Blocks (via STM retry) when no pending repos available
-}
popNextPendingRepo :: Eff AppStack RepoName
popNextPendingRepo = do
  st <- asks (.refreshState)
  now <- liftIO getCurrentTime
  atomically $ do
    statusMap <- readTVar st.statusMap
    -- Find all pending repos sorted by priority
    let pending = [(repo, prio) | (repo, Pending _ prio) <- Map.toList statusMap]
        sorted = sortWith (Down . snd) pending -- Now > Normal
    case sorted of
      [] -> retry -- Block until status map changes
      (repo, _) : _ -> do
        -- Mark as InProgress and return
        modifyTVar' st.statusMap $ Map.insert repo (InProgress now)
        pure repo

-- | Refresh a single repository (expects repo to already be marked InProgress)
refreshRepo :: Repo -> Eff AppStack ()
refreshRepo repo = withLogContext [("repo", show repo.name)] $ do
  log Info "Starting refresh"

  -- Run the actual refresh
  startTime <- liftIO getCurrentTime
  result <- runErrorNoCallStack @Text $ do
    supervisor <- asks (.supervisor)
    let mirrorPath = Workspace.mirrorPath supervisor repo.name
    Mirror.syncMirror repo.cloneUrl mirrorPath
    newBranches <- Git.remoteBranchesFromClone mirrorPath
    updateRepoBranches repo.name newBranches
  endTime <- liftIO getCurrentTime

  -- Update status based on result
  let duration = diffUTCTime endTime startTime
      refreshResult =
        RefreshResult
          { completedAt = endTime
          , duration = duration
          , outcome = case result of
              Left err -> Failure err
              Right () -> Success
          }

  -- Update TVar status and persist to acid-state
  markRepoCompleted repo.name refreshResult

  -- Log completion
  case result of
    Left err -> log Error $ "❌ Refresh failed (took " <> formatDuration duration <> "): " <> err
    Right () -> log Info $ "✅ Refresh succeeded (took " <> formatDuration duration <> ")"

-- | Update the repo with given branches, but only if there are changes
updateRepoBranches :: (Reader ViraRuntimeState :> es, IOE :> es) => RepoName -> Map BranchName Commit -> Eff es ()
updateRepoBranches repo branches = do
  current <- App.query $ St.GetRepoBranchesA repo
  let currentMap = Map.fromList [(b.branchName, b.headCommit) | b <- current, not b.deleted]
  when (currentMap /= branches) $ do
    App.update $ St.SetRepoBranchesA repo branches

-- * Helper functions for statusMap updates

-- | Mark a repository as Completed and persist to acid-state
markRepoCompleted :: (Reader ViraRuntimeState :> es, Concurrent :> es, IOE :> es) => RepoName -> RefreshResult -> Eff es ()
markRepoCompleted repo result = do
  st <- asks (.refreshState)
  -- Update TVar
  atomically $ modifyTVar' st.statusMap $ Map.insert repo (Completed result)
  -- Persist to acid-state
  App.update $ St.SetRefreshStatusA repo (Just result)

-- | Remove a repository from refresh state (cleanup on deletion)
removeRepoFromRefreshState :: (Reader ViraRuntimeState :> es, Concurrent :> es) => RepoName -> Eff es ()
removeRepoFromRefreshState repo = do
  st <- asks (.refreshState)
  atomically $ modifyTVar' st.statusMap (Map.delete repo)
