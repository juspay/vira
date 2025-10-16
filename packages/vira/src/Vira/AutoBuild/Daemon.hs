{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Auto-build daemon for automatic job triggering
module Vira.AutoBuild.Daemon (
  startAutoBuildDaemon,
) where

import Control.Concurrent.STM (retry)
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Effectful (Eff)
import Effectful.Colog.Simple (Severity (..), log, tagCurrentThread, withLogContext)
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically)
import Effectful.Git.Types (Commit (..))
import Effectful.Reader.Dynamic (asks)
import GHC.IO.Exception (ExitCode (..))
import Vira.App.AcidState qualified as AppAcid
import Vira.App.Broadcast.Core qualified as Broadcast
import Vira.App.Broadcast.Type (BroadcastScope (..))
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.AutoBuild.Type (AutoBuildState (..), BuildRequest (..))
import Vira.CI.Environment (ViraEnvironment (..), environmentFor)
import Vira.CI.Pipeline qualified as Pipeline
import Vira.CI.Workspace qualified as Workspace
import Vira.State.Acid (AddNewJobA (..), GetBranchByNameA (..), GetRepoByNameA (..), GetRunningJobs (..), JobUpdateStatusA (..))
import Vira.State.Type qualified as St
import Vira.Supervisor.Task qualified as Supervisor
import Vira.Supervisor.Type (Terminated (Terminated))
import Prelude hiding (asks, atomically)

-- | Start the auto-build daemon
startAutoBuildDaemon :: Eff AppStack ()
startAutoBuildDaemon = do
  log Info "🤖 Auto-build daemon starting..."
  workerHandle <- async workerLoop
  st <- asks (.autoBuildState)
  atomically $ writeTVar st.daemonHandle (Just workerHandle)
  log Info "🤖 Auto-build daemon started"

-- | Worker loop: continuously process pending builds
workerLoop :: Eff AppStack Void
workerLoop = do
  tagCurrentThread "🤖"
  infinitely $ do
    -- Pop next pending build (blocks via STM retry until available)
    buildReq <- popNextPendingBuild

    -- Execute the build
    withLogContext [("repo", show buildReq.repo), ("branch", show buildReq.branch)] $ do
      executeBuild buildReq

    -- Small delay to prevent tight loop
    threadDelay 100000 -- 100ms

{- | Atomically pop the next pending build
Blocks (via STM retry) when no pending builds available or concurrent limit reached
-}
popNextPendingBuild :: Eff AppStack BuildRequest
popNextPendingBuild = do
  st <- asks (.autoBuildState)
  atomically $ do
    pending <- readTVar st.pendingBuilds
    case sortBy comparePriority (Map.toList pending) of
      [] -> retry -- Block until builds available
      ((key, req) : _) -> do
        -- Remove from pending map
        modifyTVar' st.pendingBuilds $ Map.delete key
        pure req
  where
    -- Sort by priority (Manual > Auto) then by queued time
    comparePriority (_, a) (_, b) =
      case compare (Down a.priority) (Down b.priority) of
        EQ -> compare a.queuedAt b.queuedAt
        other -> other

-- | Execute a build request
executeBuild :: BuildRequest -> Eff AppStack ()
executeBuild req = do
  log Info $ "Starting auto-build for commit " <> show req.commit

  -- Verify branch still exists and has this commit
  AppAcid.query (GetBranchByNameA req.repo req.branch) >>= \case
    Nothing -> do
      log Warning "Branch no longer exists, skipping build"
      pass
    Just branch -> do
      if branch.headCommit.id /= req.commit
        then do
          log Info "Branch HEAD has moved, skipping outdated build"
          pass
        else do
          -- Check concurrent build limit
          st <- asks (.autoBuildState)
          canBuild <- checkConcurrentLimit st
          if not canBuild
            then do
              -- Re-queue the build request
              log Info "Concurrent build limit reached, re-queuing"
              atomically $ modifyTVar' st.pendingBuilds $ Map.insert (req.repo, req.branch) req
              threadDelay 5000000 -- Wait 5s before retry
            else do
              -- Create and start the build job directly
              repo <-
                AppAcid.query (GetRepoByNameA req.repo) >>= \case
                  Nothing -> do
                    log Warning "Repo not found, skipping build"
                    pure Nothing
                  Just r -> pure (Just r)

              whenJust repo $ \r -> do
                supervisor <- asks (.supervisor)
                creationTime <- liftIO getCurrentTime
                let baseDir = Workspace.repoJobsDir supervisor r.name
                job <- AppAcid.update $ AddNewJobA req.repo req.branch req.commit baseDir creationTime

                withLogContext [("job", show job.jobId)] $ do
                  log Info $ "Building commit " <> show req.commit
                  viraEnv@(ViraEnvironment {workspacePath}) <- environmentFor r branch job.jobWorkingDir
                  Supervisor.startTask
                    supervisor
                    job.jobId
                    st.logLevel
                    workspacePath
                    (Pipeline.runPipeline viraEnv)
                    $ \result -> do
                      endTime <- liftIO getCurrentTime
                      let status = case result of
                            Right ExitSuccess -> St.JobFinished St.JobSuccess endTime
                            Right (ExitFailure _code) -> St.JobFinished St.JobFailure endTime
                            Left (Pipeline.PipelineTerminated Terminated) -> St.JobFinished St.JobKilled endTime
                            Left _ -> St.JobFinished St.JobFailure endTime
                      AppAcid.update $ JobUpdateStatusA job.jobId status
                      Broadcast.broadcastUpdate (JobScope job.jobId)
                  AppAcid.update $ JobUpdateStatusA job.jobId St.JobRunning
                  Broadcast.broadcastUpdate (JobScope job.jobId)
                  log Info "Started task"

                  -- Update last built commit
                  atomically $ modifyTVar' st.lastBuiltCommit $ Map.insert (req.repo, req.branch) req.commit

-- | Check if we can start a new build without exceeding concurrent limit
checkConcurrentLimit :: AutoBuildState -> Eff AppStack Bool
checkConcurrentLimit st = do
  if st.maxConcurrentBuilds == 0
    then pure True -- No limit
    else do
      runningJobs <- AppAcid.query GetRunningJobs
      let currentCount = fromIntegral $ length runningJobs
      pure $ currentCount < st.maxConcurrentBuilds
