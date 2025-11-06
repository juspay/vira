{-# LANGUAGE OverloadedRecordDot #-}

{- | Refresh daemon for automatic repository updates

This module provides a background daemon that continuously refreshes git repositories.
It consists of three concurrent workers:
- Scheduler: periodically schedules all 'Vira.State.Type.Repo's for refresh
- Cleanup: subscribes to repo deletion events and cleans up state
- Worker: processes pending repos and performs the actual 'refreshRepo' operations
-}
module Vira.Refresh.Daemon (
  startRefreshDaemon,
) where

import Colog.Message (RichMessage)
import Control.Concurrent.STM.TChan (readTChan)
import Data.Acid.Events qualified as Events
import Data.Map.Strict qualified as Map
import Data.Time (diffUTCTime, getCurrentTime)
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, Severity (..), log, tagCurrentThread, withLogContext)
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Git (BranchName, Commit (..), CommitID (..), RepoName (..))
import Effectful.Git.Command.ForEachRef qualified as Git
import Effectful.Git.Mirror qualified as Mirror
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful.Reader.Static qualified as ER
import Vira.App.AcidState qualified as App
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.CI.Workspace qualified as Workspace
import Vira.Lib.TimeExtra (formatDuration)
import Vira.Refresh (scheduleRepoRefresh)
import Vira.Refresh.State qualified as State
import Vira.Refresh.Type (RefreshOutcome (..), RefreshPriority (..), RefreshResult (..), RefreshState (..))
import Vira.State.Acid (BranchUpdate (..), DeleteRepoByNameA (..), GetAllReposA (..), GetRepoByNameA (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (Branch (..), Repo (..))
import Prelude hiding (Reader, asks, atomically)

{- | Start the refresh daemon

Initializes the 'Vira.Refresh.Type.RefreshState' from persisted data and spawns
three concurrent workers (scheduler, cleanup, worker).
-}
startRefreshDaemon :: Eff AppStack ()
startRefreshDaemon = do
  log Info "🔄 Refresh daemon starting..."

  -- Initialize refresh state from persisted data
  repos <- App.query GetAllReposA
  refreshState <- asks (.refreshState)
  State.initialize refreshState repos
  log Info "🔄 Loaded refresh status from acid-state"

  void $ async schedulerLoop
  void $ async cleanupWorker
  workerHandle <- async workerLoop

  st <- asks (.refreshState)
  atomically $ writeTVar st.daemonHandle (Just workerHandle)

  log Info "🔄 Refresh daemon started (scheduler + cleanup + worker)"

{- | Scheduler loop: periodically set all repos to 'Vira.Refresh.Type.Pending' with 'Vira.Refresh.Type.Normal' priority

Runs every 5 minutes and schedules all 'Vira.State.Type.Repo's for refresh.
-}
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

{- | Cleanup worker: subscribe to repo deletion events and clean up refresh state

Subscribes to 'Vira.State.Acid.DeleteRepoByNameA' events and removes the repo from 'Vira.Refresh.Type.RefreshState'.
-}
cleanupWorker :: Eff AppStack Void
cleanupWorker = do
  tagCurrentThread "🔄"
  chan <- App.subscribe

  infinitely $ do
    someUpdate <- atomically $ readTChan chan
    whenJust (Events.matchUpdate someUpdate) $ \(DeleteRepoByNameA name, _) -> do
      log Info $ "Repo deleted, cleaning up refresh state: " <> show name
      refreshState <- asks (.refreshState)
      State.remove refreshState name

{- | Worker loop: continuously process pending repos

Pops the next pending 'Vira.State.Type.Repo' from the queue (blocks via STM retry until available)
and calls 'refreshRepo' to perform the actual refresh operation.
-}
workerLoop :: Eff AppStack Void
workerLoop = do
  tagCurrentThread "🔄"
  st <- asks (.refreshState)
  infinitely $ do
    -- Pop next pending repo (blocks via STM retry until available)
    repoName <- State.popAndMarkInProgress st

    -- Fetch repo data and refresh
    App.query (GetRepoByNameA repoName) >>= \case
      Nothing -> do
        -- Should not happen - cleanup worker should remove deleted repos
        log Error $ "Repo not found in state (cleanup worker should have removed it): " <> show repoName
      Just repo -> refreshRepo repo

{- | Refresh a single repository (expects repo to already be marked 'Vira.Refresh.Type.InProgress')

Performs the following operations:
1. Syncs the git mirror using 'Effectful.Git.Mirror.syncMirror'
2. Fetches remote branches via 'Effectful.Git.Command.ForEachRef.remoteBranchesFromClone'
3. Updates 'Vira.State.Type.Branch'es in acid-state via 'updateRepoBranches'
4. Records the 'Vira.Refresh.Type.RefreshResult' in both 'Vira.Refresh.Type.RefreshState' and acid-state
-}
refreshRepo :: Repo -> Eff AppStack ()
refreshRepo repo = withLogContext [("repo", show repo.name)] $ do
  log Debug "Starting refresh"

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

  -- Update TVar status
  st <- asks (.refreshState)
  State.markCompleted st repo.name refreshResult

  -- Persist to acid-state
  App.update $ St.SetRefreshStatusA repo.name (Just refreshResult)

  -- Log completion
  case result of
    Left err -> log Error $ "❌ Refresh failed (took " <> formatDuration duration <> "): " <> err
    Right () -> log Debug $ "✅ Refresh succeeded (took " <> formatDuration duration <> ")"

{- | Update the 'Vira.State.Type.Repo' with given 'Vira.State.Type.Branch'es, but only if there are changes

Compares the current branches with the new branches and only calls 'Vira.State.Acid.SetRepoBranchesA'
if there are actual changes to avoid unnecessary acid-state updates.
-}
updateRepoBranches ::
  (Reader ViraRuntimeState :> es, IOE :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es) =>
  RepoName -> Map BranchName Commit -> Eff es ()
updateRepoBranches repo branches = do
  current <- App.query $ St.GetRepoBranchesA repo
  let currentMap = Map.fromList [(b.branchName, b.headCommit) | b <- current, not b.deleted]
  when (currentMap /= branches) $ do
    updates <- App.update $ St.SetRepoBranchesA repo branches
    forM_ updates $ \upd ->
      log Info $
        "🪵 "
          <> toText upd.branch
          <> ": "
          <> maybe "new" (\c -> c.id.unCommitID) upd.oldCommit
          <> " → "
          <> upd.newCommit.id.unCommitID
