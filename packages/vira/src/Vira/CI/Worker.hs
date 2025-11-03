{-# LANGUAGE OverloadedRecordDot #-}

{- | CI job worker daemon for queue processing

This module provides a background daemon that processes CI job queue with concurrency limits.
It subscribes to job events (new jobs, status changes) and starts pending jobs when slots available.
-}
module Vira.CI.Worker (
  startJobWorkerDaemon,

  -- * Exported for testing
  selectJobsToStart,
) where

import Control.Concurrent.STM.TChan (readTChan)
import Data.Acid.Events qualified as Events
import Data.Time (getCurrentTime)
import Effectful (Eff)
import Effectful.Colog.Simple (Severity (..), log, tagCurrentThread)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically)
import Effectful.Reader.Dynamic (ask)
import System.Exit (ExitCode (..))
import Vira.App.AcidState qualified as App
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (jobWorker, supervisor))
import Vira.CI.Pipeline qualified as Pipeline
import Vira.CI.Pipeline.Program qualified as Program
import Vira.CI.Worker.Type (JobWorkerState (..))
import Vira.Environment.Tool.Core qualified as Tool
import Vira.State.Acid (AddNewJobA (..), GetBranchByNameA (..), GetPendingJobsA (..), GetRepoByNameA (..), GetRunningJobs (..), JobUpdateStatusA (..))
import Vira.State.Type (Job (..), JobStatus (..))
import Vira.State.Type qualified as St
import Vira.Supervisor.Task qualified as Supervisor
import Vira.Supervisor.Type (Terminated (..))
import Prelude hiding (ask, atomically)

{- | Start the job worker daemon

Spawns a background worker that subscribes to job events and starts pending jobs.
-}
startJobWorkerDaemon :: Eff AppStack ()
startJobWorkerDaemon = do
  log Info "🔨 Job worker daemon starting..."
  void $ async workerLoop
  log Info "🔨 Job worker daemon started"

{- | Worker loop: subscribe to job events and start pending jobs

Subscribes to 'AddNewJobA' (new job created) and 'JobUpdateStatusA' (job status changed) events.
When relevant event occurs, calls 'tryStartPendingJobs' to fill available slots.
-}
workerLoop :: Eff AppStack Void
workerLoop = do
  tagCurrentThread "🔨"
  chan <- App.subscribe

  infinitely $ do
    someUpdate <- atomically $ readTChan chan
    -- Check if it's AddNewJobA event
    case Events.matchUpdate @AddNewJobA someUpdate of
      Just _ -> tryStartPendingJobs
      Nothing -> pass
    -- Check if it's JobUpdateStatusA with JobFinished
    case Events.matchUpdate @JobUpdateStatusA someUpdate of
      Just (JobUpdateStatusA _ (JobFinished _ _), _) -> tryStartPendingJobs
      _ -> pass

{- | Try to start pending jobs if slots available

Queries active jobs (pending + running), uses 'selectJobsToStart' to pick which to start,
then calls 'startJob' for each selected job.
-}
tryStartPendingJobs :: Eff AppStack ()
tryStartPendingJobs = do
  -- Get all active jobs
  pending <- App.query GetPendingJobsA
  running <- App.query GetRunningJobs
  let activeJobs = pending <> running

  -- Select jobs to start
  st <- ask @ViraRuntimeState
  let toStart = selectJobsToStart st.jobWorker.maxConcurrent activeJobs

  -- Start selected jobs
  forM_ toStart startJob

{- | Pure job selection logic (FIFO queue with concurrency limit)

Given max concurrent limit and active jobs, returns which pending jobs should start.
Respects concurrency limit and starts oldest jobs first (FIFO).

This is a pure function to enable unit testing.
-}
selectJobsToStart ::
  -- | Maximum concurrent jobs
  Int ->
  -- | All active jobs (pending + running)
  [Job] ->
  -- | Jobs to start now
  [Job]
selectJobsToStart maxConcurrent activeJobs =
  let runningCount = length [j | j <- activeJobs, j.jobStatus == JobRunning]
      availableSlots = maxConcurrent - runningCount
      pendingJobs = filter (\j -> j.jobStatus == JobPending) activeJobs
      sorted = sortOn (.jobCreatedTime) pendingJobs -- FIFO
   in take availableSlots sorted

{- | Start a single job (extracted from JobPage.hs triggerNewBuild)

Calls supervisor to run the job, sets up completion callback, marks as running.
-}
startJob :: Job -> Eff AppStack ()
startJob job = do
  log Info $ "Starting job #" <> show job.jobId

  -- Get repo and branch
  repo <-
    App.query (GetRepoByNameA job.repo) >>= \case
      Nothing -> do
        log Error $ "Repo not found for job #" <> show job.jobId
        error "Repo not found"
      Just r -> pure r

  branch <-
    App.query (GetBranchByNameA job.repo job.branch) >>= \case
      Nothing -> do
        log Error $ "Branch not found for job #" <> show job.jobId
        error "Branch not found"
      Just b -> pure b

  -- Start task
  st <- ask @ViraRuntimeState
  tools <- Tool.refreshTools
  Supervisor.startTask
    st.supervisor
    job.jobId
    Info
    job.jobWorkingDir
    (\logger -> Pipeline.runPipeline (Pipeline.pipelineEnvFromRemote branch job.jobWorkingDir tools logger) (Program.pipelineProgramWithClone repo branch job.jobWorkingDir))
    $ \result -> do
      endTime <- liftIO getCurrentTime
      let status = case result of
            Right ExitSuccess -> JobFinished St.JobSuccess endTime
            Right (ExitFailure _code) -> JobFinished St.JobFailure endTime
            Left (Pipeline.PipelineTerminated Terminated) -> JobFinished St.JobKilled endTime
            Left _ -> JobFinished St.JobFailure endTime
      -- Update status
      App.update $ JobUpdateStatusA job.jobId status

  -- Mark as running
  App.update $ JobUpdateStatusA job.jobId JobRunning
  log Info $ "Started job #" <> show job.jobId
