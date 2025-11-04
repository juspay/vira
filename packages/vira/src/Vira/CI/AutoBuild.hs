{-# LANGUAGE OverloadedRecordDot #-}

{- | Auto-build daemon that triggers CI jobs when branches update

Subscribes to branch update events and automatically enqueues builds,
with smart cancellation of pending jobs for skipped commits.
-}
module Vira.CI.AutoBuild (
  startAutoBuildDaemon,
  selectJobsToCancel,
) where

import Control.Concurrent.STM.TChan (readTChan)
import Data.Acid.Events qualified as Events
import Data.Time (getCurrentTime)
import Effectful (Eff)
import Effectful.Colog.Simple (Severity (..), log, tagCurrentThread)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically)
import Effectful.Git (BranchName, RepoName)
import Vira.App.AcidState qualified as App
import Vira.App.Stack (AppStack)
import Vira.CI.Client qualified as Client
import Vira.State.Acid (BranchUpdate (..), CancelPendingJobA (..), GetJobsByBranchA (..), SetRepoBranchesA (..))
import Vira.State.Type (Job (..), JobStatus (..))
import Prelude hiding (atomically)

{- | Start the auto-build daemon

Spawns a background worker that subscribes to branch update events
and automatically enqueues builds for new commits.
-}
startAutoBuildDaemon :: Eff AppStack ()
startAutoBuildDaemon = do
  log Info "🚀 Auto-build daemon starting..."
  void $ async autoBuildLoop
  log Info "🚀 Auto-build daemon started"

{- | Auto-build loop: subscribe to branch update events and enqueue builds

Subscribes to 'SetRepoBranchesA' events (branch updates from refresh daemon).
For each branch update, cancels pending jobs and enqueues a new build.
-}
autoBuildLoop :: Eff AppStack Void
autoBuildLoop = do
  tagCurrentThread "🚀"
  chan <- App.subscribe

  infinitely $ do
    someUpdate <- atomically $ readTChan chan
    -- Use the event result (diff) returned by setRepoBranchesA
    whenJust (Events.matchUpdate @SetRepoBranchesA someUpdate) $ \(SetRepoBranchesA repo _branches, updates) ->
      handleBranchUpdates repo updates

-- | Handle branch updates by cancelling old pending jobs and enqueueing new ones
handleBranchUpdates :: RepoName -> [BranchUpdate] -> Eff AppStack ()
handleBranchUpdates repo updates = do
  forM_ updates $ \upd -> do
    -- Cancel pending jobs for this branch
    cancelPendingJobsForBranch repo upd.branch
    -- Enqueue job for new commit
    Client.enqueueJob repo upd.branch upd.newCommit

-- | Cancel all pending jobs for a specific branch
cancelPendingJobsForBranch :: RepoName -> BranchName -> Eff AppStack ()
cancelPendingJobsForBranch repo branch = do
  jobs <- App.query $ GetJobsByBranchA repo branch
  now <- liftIO getCurrentTime
  let toCancel = selectJobsToCancel jobs repo (BranchUpdate branch Nothing "") -- repo and update params not used in filter
  forM_ toCancel $ \job ->
    void $ App.update $ CancelPendingJobA job.jobId now

{- | Select jobs that should be cancelled (exported for testing)

Returns all pending jobs for the given repo/branch.
-}
selectJobsToCancel :: [Job] -> RepoName -> BranchUpdate -> [Job]
selectJobsToCancel jobs repo upd =
  filter
    ( \j ->
        j.repo == repo
          && j.branch == upd.branch
          && j.jobStatus == JobPending
    )
    jobs
