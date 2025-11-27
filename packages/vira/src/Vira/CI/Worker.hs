{-# LANGUAGE OverloadedRecordDot #-}

{- | CI job worker daemon for queue processing

This module provides a background daemon that processes CI job queue with concurrency limits.
It subscribes to job events (new jobs, status changes) and starts pending jobs when slots available.
-}
module Vira.CI.Worker (
  startJobWorkerDaemon,
  tryStartPendingJobs,

  -- * Exported for testing
  selectJobsToStart,
) where

import Colog.Message (RichMessage)
import Control.Concurrent.STM.TChan (readTChan)
import Data.Acid.Events qualified as Events
import Data.Set qualified as Set
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, Severity (..), log, tagCurrentThread)
import Effectful.Concurrent.Async (Concurrent, async)
import Effectful.Concurrent.MVar (withMVar)
import Effectful.Concurrent.STM (atomically)
import Effectful.Environment (Environment)
import Effectful.FileSystem (FileSystem)
import Effectful.Git.Types (Commit (id))
import Effectful.Process (Process)
import Effectful.Reader.Dynamic (Reader, ask)
import Effectful.Reader.Static qualified as ER
import System.Exit (ExitCode (..))
import Vira.App.AcidState qualified as App
import Vira.App.Stack (AppStack)
import Vira.App.Type (ViraRuntimeState (jobWorker, supervisor))
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Pipeline qualified as Pipeline
import Vira.CI.Pipeline.Program qualified as Program
import Vira.CI.Worker.Type (JobWorkerState (..))
import Vira.Environment.Tool.Core qualified as Tool
import Vira.State.Acid
import Vira.State.Acid qualified as Acid
import Vira.State.Type (Job (..), JobStatus (..))
import Vira.State.Type qualified as St
import Vira.Supervisor.Task qualified as Supervisor
import Vira.Supervisor.Type (Terminated (..))
import Prelude hiding (Reader, ask, atomically)

{- | Start the job worker daemon

Spawns a background worker that subscribes to job events and starts pending jobs.
-}
startJobWorkerDaemon :: Eff AppStack ()
startJobWorkerDaemon = do
  log Info "🔨 Job worker daemon starting..."
  void $ async workerLoop
  log Info "🔨 Job worker daemon started"

{- | Worker loop: subscribe to job events and start pending jobs

Subscribes to 'JobUpdateStatusA' (job status changed) events.
When a job finishes, calls 'tryStartPendingJobs' to fill available slots.

Note: New jobs are scheduled synchronously in 'enqueueJob', so no listener needed.
-}
workerLoop :: Eff AppStack Void
workerLoop = do
  tagCurrentThread "🔨"
  chan <- App.subscribe

  infinitely $ do
    someUpdate <- atomically $ readTChan chan
    -- When any job finishes, try to start pending jobs
    whenJust (Events.matchUpdate @JobUpdateStatusA someUpdate) $ \case
      (JobUpdateStatusA _ (JobFinished _ _), _) -> tryStartPendingJobs
      _ -> pass

{- | Try to start pending jobs if slots available

Queries active jobs (pending + running), uses 'selectJobsToStart' to pick which to start,
then calls 'startJob' for each selected job.

Protected by scheduler lock to prevent race conditions.
-}
tryStartPendingJobs ::
  ( Reader ViraRuntimeState :> es
  , Concurrent :> es
  , Process :> es
  , FileSystem :> es
  , Environment :> es
  , ER.Reader LogContext :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  ) =>
  Eff es ()
tryStartPendingJobs = do
  jobWorker <- asks (.jobWorker)

  withMVar jobWorker.schedulerLock $ \_ -> do
    activeJobs <- App.query GetActiveJobsA
    let toStart = selectJobsToStart jobWorker.maxConcurrent activeJobs
    startJob `mapM_` toStart

{- | Pure job selection logic (FIFO queue with concurrency limit + branch dedup)

Selects which pending jobs to start based on:
- Concurrency limit (max total running jobs)
- Branch deduplication (max 1 running job per repo/branch pair)
- FIFO ordering (oldest pending jobs first)

This is a pure function to enable unit testing.
-}
selectJobsToStart ::
  -- | Maximum concurrent jobs
  Int ->
  -- | Active jobs (running + pending)
  Acid.ActiveJobs ->
  -- | Pending jobs selected to start
  [Job]
selectJobsToStart maxConcurrent activeJobs =
  activeJobs.pending
    & filter (\j -> not $ (j.repo, j.branch) `Set.member` runningBranches) -- Max 1 job per branch
    & sortOn (.jobCreatedTime) -- FIFO
    & take availableSlots -- Fill available slots
  where
    availableSlots = max 0 (maxConcurrent - length activeJobs.running)
    runningBranches = Set.fromList $ activeJobs.running <&> (\j -> (j.repo, j.branch))

{- | Start a single job (extracted from JobPage.hs triggerNewBuild)

Calls supervisor to run the job, sets up completion callback, marks as running.
-}
startJob ::
  ( Reader ViraRuntimeState :> es
  , Concurrent :> es
  , Process :> es
  , FileSystem :> es
  , Environment :> es
  , ER.Reader LogContext :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  ) =>
  Job ->
  Eff es ()
startJob job = do
  log Info $ "Starting job #" <> show job.jobId

  -- Get repo and branch
  repo <-
    App.query (GetRepoByNameA job.repo) >>= \case
      Nothing -> error "Repo not found; impossible!"
      Just r -> pure r

  branch <-
    App.query (GetBranchByNameA job.repo job.branch) >>= \case
      Nothing -> error "Branch not found; impossible!"
      Just b -> pure b

  -- Start task
  st <- ask @ViraRuntimeState
  tools <- Tool.refreshTools
  let ctx = ViraContext job.branch False branch.headCommit.id repo.cloneUrl job.jobWorkingDir
  Supervisor.startTask
    st.supervisor
    job.jobId
    st.jobWorker.minSeverity
    job.jobWorkingDir
    ( \logger ->
        Pipeline.runPipeline
          (Pipeline.pipelineEnvFromRemote tools logger ctx)
          (Program.pipelineProgramWithClone repo branch job.jobWorkingDir)
    )
    $ \result -> do
      endTime <- liftIO getCurrentTime
      let status = case result of
            Right ExitSuccess -> JobFinished St.JobSuccess endTime
            Right (ExitFailure _code) -> JobFinished St.JobFailure endTime
            Left (Pipeline.PipelineTerminated Terminated) -> JobFinished St.JobKilled endTime
            Left _ -> JobFinished St.JobFailure endTime
      -- Update status
      void $ App.update $ JobUpdateStatusA job.jobId status

  -- Mark as running
  void $ App.update $ JobUpdateStatusA job.jobId JobRunning
  log Info $ "Started job #" <> show job.jobId
