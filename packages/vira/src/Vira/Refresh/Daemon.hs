{-# LANGUAGE OverloadedRecordDot #-}

-- | Refresh daemon for automatic repository updates
module Vira.Refresh.Daemon (
  startRefreshDaemon,
) where

import Control.Concurrent.STM (retry)
import Data.Acid qualified as Acid
import Data.Map.Strict qualified as Map
import Data.Time (diffUTCTime, getCurrentTime)
import Effectful (Eff)
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Git (RepoName (..))
import Effectful.Git qualified as Git
import Effectful.Git.Mirror qualified as Mirror
import Effectful.Reader.Dynamic (asks)
import Vira.App.AcidState qualified as App
import Vira.App.Broadcast.Core qualified as Broadcast
import Vira.App.Broadcast.Type (BroadcastScope (..))
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.CI.Workspace qualified as Workspace
import Vira.Lib.Logging (Severity (..), log, tagCurrentThread, withLogContext)
import Vira.Lib.TimeExtra (formatDuration)
import Vira.Refresh.Core (initializeRefreshState, scheduleRepoRefresh)
import Vira.Refresh.Type (RefreshOutcome (..), RefreshPriority (..), RefreshResult (..), RefreshState (..), RefreshStatus (..))
import Vira.State.Acid (GetAllReposA (..), GetRepoByNameA (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (Repo (..))
import Prelude hiding (asks, atomically)

-- | Start the refresh daemon
startRefreshDaemon :: Eff AppStack ()
startRefreshDaemon = do
  log Info "🔄 Refresh daemon starting..."

  -- Initialize refresh state from persisted data
  initializeRefreshState
  log Info "🔄 Loaded refresh status from acid-state"

  void $ async schedulerLoop
  workerHandle <- async workerLoop

  st <- asks (.refreshState)
  atomically $ writeTVar st.daemonHandle (Just workerHandle)

  log Info "🔄 Refresh daemon started (scheduler + worker)"

-- | Scheduler loop: periodically set all repos to Pending with Normal priority
schedulerLoop :: Eff AppStack Void
schedulerLoop = do
  tagCurrentThread "🔄"
  infinitely $ do
    repos <- App.query GetAllReposA
    log Info $ "Scheduling refresh for " <> show (length repos) <> " repos"
    forM_ repos $ \repo ->
      scheduleRepoRefresh repo.name Normal
    threadDelay (5 * 60 * 1000000) -- 5 minutes in microseconds

-- | Worker loop: continuously process pending repos
workerLoop :: Eff AppStack Void
workerLoop = do
  tagCurrentThread "🔄"
  infinitely $ do
    -- Pop next pending repo (blocks via STM retry until available)
    repoName <- popNextPendingRepo

    -- Fetch repo data and refresh
    App.query (GetRepoByNameA repoName) >>= \case
      Nothing -> log Warning $ "Repo not found: " <> show repoName
      Just repo -> void $ refreshRepo repo

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
refreshRepo :: Repo -> Eff AppStack (Either Text ())
refreshRepo repo = withLogContext [("repo", show repo.name)] $ do
  log Info "Starting refresh"

  st <- asks (.refreshState)
  acid <- asks (.acid)
  supervisor <- asks (.supervisor)
  startTime <- liftIO getCurrentTime

  -- Run the actual refresh
  let mirrorPath = Workspace.mirrorPath supervisor repo.name
  result <- runErrorNoCallStack @Text $ do
    Mirror.syncMirror repo.cloneUrl mirrorPath
    allBranches <- Git.remoteBranchesFromClone mirrorPath
    liftIO $ Acid.update acid $ St.SetRepoBranchesA repo.name allBranches

  -- Update status based on result
  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime
      durationText = formatDuration duration
      refreshResult =
        RefreshResult
          { completedAt = endTime
          , duration = duration
          , outcome = case result of
              Left err -> Failure err
              Right () -> Success
          }

  -- Update TVar status
  atomically $
    modifyTVar' st.statusMap $
      Map.insert repo.name (Completed refreshResult)

  -- Update repo.lastRefresh in acid-state and broadcast
  let updatedRepo = repo {lastRefresh = Just refreshResult}
  App.update (St.SetRepoA updatedRepo)
  Broadcast.broadcastUpdate (RepoScope repo.name)

  -- Log completion
  case result of
    Left err -> log Error $ "❌ Refresh failed (took " <> durationText <> "): " <> err
    Right () -> log Info $ "✅ Refresh succeeded (took " <> durationText <> ")"

  pure result
