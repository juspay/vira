{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Show and AffectedEntities instances for acid-state Update types
module Vira.App.Event.Instances () where

import Data.Set qualified as Set
import Vira.App.Event.Entity (AffectedEntities (..), EntityId (..))
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
  affectedEntities (SetAllReposA repos) _ =
    Set.fromList $ fmap (\r -> RepoId r.name) repos

instance AffectedEntities AddNewRepoA where
  affectedEntities (AddNewRepoA repo) _ =
    one (RepoId repo.name)

instance AffectedEntities DeleteRepoByNameA where
  affectedEntities (DeleteRepoByNameA name) (Right ()) =
    one (RepoId name)
  affectedEntities _ _ = Set.empty

instance AffectedEntities SetRepoA where
  affectedEntities (SetRepoA repo) _ =
    one (RepoId repo.name)

instance AffectedEntities SetRepoBranchesA where
  affectedEntities (SetRepoBranchesA name _) _ =
    one (RepoId name)

instance AffectedEntities StoreCommitA where
  -- Commits don't have direct entity scoping for SSE
  affectedEntities _ _ = Set.empty

instance AffectedEntities AddNewJobA where
  affectedEntities (AddNewJobA repo _ _ _ _) job =
    Set.fromList [RepoId repo, JobId job.jobId]

instance AffectedEntities JobUpdateStatusA where
  affectedEntities (JobUpdateStatusA jid _) _ =
    one (JobId jid)

instance AffectedEntities MarkUnfinishedJobsAsStaleA where
  -- This is internal, no SSE needed
  affectedEntities _ _ = Set.empty
