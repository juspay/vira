{-# LANGUAGE OverloadedRecordDot #-}

{- | Workspace path management for Vira CI.

This module provides functions to construct filesystem paths for repositories,
jobs, and mirrors in a consistent way across the application.

All paths are derived from the 'Vira.Supervisor.Type.TaskSupervisor' @baseWorkDir@.
-}
module Vira.CI.Workspace (
  mirrorPath,
  repoJobsDir,
  repoDir,
) where

import Effectful.Git (RepoName)
import System.FilePath ((</>))
import Vira.Supervisor.Type (TaskSupervisor (..))

-- | Get the mirror/source directory for a repository (@{baseWorkDir}/{repoName}/source@)
mirrorPath :: TaskSupervisor -> RepoName -> FilePath
mirrorPath sup name = repoDir sup name </> "source"

-- | Get the base directory for all jobs of a repository (@{baseWorkDir}/{repoName}/jobs@)
repoJobsDir :: TaskSupervisor -> RepoName -> FilePath
repoJobsDir sup name = repoDir sup name </> "jobs"

-- | Get the repository base directory (@{baseWorkDir}/{repoName}@)
repoDir :: TaskSupervisor -> RepoName -> FilePath
repoDir sup name = sup.baseWorkDir </> toString name
