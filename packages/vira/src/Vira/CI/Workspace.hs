{-# LANGUAGE OverloadedRecordDot #-}

{- |
Workspace path management for Vira CI.

This module provides functions to construct filesystem paths for repositories,
jobs, and mirrors in a consistent way across the application.
-}
module Vira.CI.Workspace (
  mirrorPath,
  repoJobsDir,
  repoDir,
) where

import Effectful.Git (RepoName)
import System.FilePath ((</>))
import Vira.Supervisor.Type (TaskSupervisor (..))

-- | Get the mirror/source directory for a repository
mirrorPath :: TaskSupervisor -> RepoName -> FilePath
mirrorPath supervisor repoName =
  repoDir supervisor repoName </> "source"

-- | Get the base directory for all jobs of a repository
repoJobsDir :: TaskSupervisor -> RepoName -> FilePath
repoJobsDir supervisor repoName =
  repoDir supervisor repoName </> "jobs"

-- | Get the repository base directory
repoDir :: TaskSupervisor -> RepoName -> FilePath
repoDir supervisor repoName =
  supervisor.baseWorkDir </> toString repoName
