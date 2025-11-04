{-# LANGUAGE OverloadedRecordDot #-}

{- | Client API for triggering CI builds

This module provides the public interface for creating and managing CI jobs.
-}
module Vira.CI.Client (
  enqueueJob,
) where

import Colog.Message (RichMessage)
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, Severity (Info), log)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Environment (Environment)
import Effectful.FileSystem (FileSystem)
import Effectful.Git (BranchName, CommitID, RepoName)
import Effectful.Process (Process)
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful.Reader.Static qualified as ER
import Vira.App.AcidState qualified as App
import Vira.App.Type (ViraRuntimeState (supervisor))
import Vira.CI.Worker qualified as Worker
import Vira.CI.Workspace qualified as Workspace
import Vira.State.Acid (AddNewJobA (..))
import Vira.State.Type (Job (..))
import Prelude hiding (Reader, asks)

{- | Create a job and queue it for the CI worker to run

This is the entry point for triggering new builds. The job is created with 'JobPending'
status and immediately scheduled (worker fills slots synchronously).
-}
enqueueJob ::
  ( Reader ViraRuntimeState :> es
  , Concurrent :> es
  , Process :> es
  , FileSystem :> es
  , Environment :> es
  , ER.Reader LogContext :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  ) =>
  RepoName ->
  BranchName ->
  CommitID ->
  Eff es ()
enqueueJob repoName branchName commitId = do
  sup <- asks supervisor
  creationTime <- liftIO getCurrentTime
  let baseDir = Workspace.repoJobsDir sup repoName

  -- Create job as Pending
  job <- App.update $ AddNewJobA repoName branchName commitId baseDir creationTime
  log Info $ "Queued job #" <> show job.jobId

  -- Immediately try to schedule it (with lock)
  Worker.tryStartPendingJobs
