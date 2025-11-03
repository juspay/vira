{-# LANGUAGE OverloadedRecordDot #-}

{- | Client API for triggering CI builds

This module provides the public interface for creating and managing CI jobs.
-}
module Vira.CI.Client (
  enqueueJob,
) where

import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, type (:>))
import Effectful.Git (BranchName, Commit (..), RepoName)
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.AcidState qualified as App
import Vira.App.Type (ViraRuntimeState (supervisor))
import Vira.CI.Workspace qualified as Workspace
import Vira.State.Acid (AddNewJobA (..))
import Vira.State.Type (Job)
import Prelude hiding (Reader, asks)

{- | Create a job and queue it for the CI worker to run

This is the entry point for triggering new builds. The job is created with 'JobPending'
status and the worker daemon will pick it up when slots are available.
-}
enqueueJob ::
  (Reader ViraRuntimeState :> es, IOE :> es) =>
  RepoName ->
  BranchName ->
  Commit ->
  Eff es Job
enqueueJob repoName branchName commit = do
  sup <- asks supervisor
  creationTime <- liftIO getCurrentTime
  let baseDir = Workspace.repoJobsDir sup repoName
  App.update $ AddNewJobA repoName branchName commit.id baseDir creationTime
