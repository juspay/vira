{-# LANGUAGE OverloadedRecordDot #-}

{- |
Workspace path management for Vira CI.

This module provides functions to construct filesystem paths for repositories,
jobs, and mirrors in a consistent way across the application.
-}
module Vira.CI.Workspace (
  mirrorPath,
  jobWorkDir,
  repoJobsDir,
  repoDir,
) where

import Effectful.Git (RepoName)
import System.FilePath ((</>))
import Vira.State.Type (JobId (..))
import Vira.Supervisor.Type (TaskSupervisor (..))

-- | Get the mirror/source directory for a repository
mirrorPath :: TaskSupervisor -> RepoName -> FilePath
mirrorPath supervisor repoName =
  supervisor.baseWorkDir </> toString repoName </> "source"

-- | Get the working directory for a specific job
jobWorkDir :: TaskSupervisor -> RepoName -> JobId -> FilePath
jobWorkDir supervisor repoName jobId =
  supervisor.baseWorkDir </> toString repoName </> "jobs" </> show jobId

-- | Get the base directory for all jobs of a repository
repoJobsDir :: TaskSupervisor -> RepoName -> FilePath
repoJobsDir supervisor repoName =
  supervisor.baseWorkDir </> toString repoName </> "jobs"

-- | Get the repository base directory
repoDir :: TaskSupervisor -> RepoName -> FilePath
repoDir supervisor repoName =
  supervisor.baseWorkDir </> toString repoName
