{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Show and AffectedEntities instances for acid-state Update types
module Vira.App.Event.Instances () where

import Vira.App.Event.Type (AffectedEntities (..))
import Vira.State.Acid
import Vira.State.Type (Job (jobId), Repo (name))

-- * Show instances (for debug logging)

deriving stock instance Show SetAllReposA

deriving stock instance Show AddNewRepoA

deriving stock instance Show DeleteRepoByNameA

deriving stock instance Show GetAllReposA

deriving stock instance Show GetRepoByNameA

deriving stock instance Show GetAllBranchesA

deriving stock instance Show GetBranchByNameA

deriving stock instance Show GetBranchDetailsA

deriving stock instance Show SetRepoA

deriving stock instance Show SetRepoBranchesA

deriving stock instance Show GetCommitByIdA

deriving stock instance Show StoreCommitA

deriving stock instance Show GetJobsByBranchA

deriving stock instance Show GetRecentJobsA

deriving stock instance Show GetRunningJobs

deriving stock instance Show GetJobA

deriving stock instance Show AddNewJobA

deriving stock instance Show JobUpdateStatusA

deriving stock instance Show MarkUnfinishedJobsAsStaleA

-- * AffectedEntities instances (for SSE filtering)

instance AffectedEntities SetAllReposA where
  affectedRepos (SetAllReposA repos) _ = fmap (\r -> r.name) repos

instance AffectedEntities AddNewRepoA where
  affectedRepos (AddNewRepoA repo) _ = [repo.name]

instance AffectedEntities DeleteRepoByNameA where
  affectedRepos (DeleteRepoByNameA name) (Right ()) = [name]
  affectedRepos _ _ = []

instance AffectedEntities SetRepoA where
  affectedRepos (SetRepoA repo) _ = [repo.name]

instance AffectedEntities SetRepoBranchesA where
  affectedRepos (SetRepoBranchesA name _) _ = [name]

instance AffectedEntities StoreCommitA where
  -- Commits don't have direct entity scoping for SSE
  affectedRepos _ _ = []

instance AffectedEntities AddNewJobA where
  affectedRepos (AddNewJobA repo _ _ _ _) _ = [repo]
  affectedJobs _ job = [job.jobId]

instance AffectedEntities JobUpdateStatusA where
  affectedJobs (JobUpdateStatusA jobId _) _ = [jobId]

instance AffectedEntities MarkUnfinishedJobsAsStaleA where
  -- This is internal, no SSE needed
  affectedRepos _ _ = []
  affectedJobs _ _ = []
