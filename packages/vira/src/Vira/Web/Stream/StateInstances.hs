{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
AffectedEntities instances for acid-state events.

These instances determine which entities are affected by each update event,
used for scoped SSE filtering. Import this module anywhere you use App.update
to bring instances into scope.
-}
module Vira.State.AcidInstances () where

import Data.Set qualified as Set
import Vira.State.Acid
import Vira.State.Type (Job (jobId), Repo (name))
import Vira.Web.Stream.AffectedEntities (AffectedEntities (..), EntityId (..))

-- * AffectedEntities instances

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

-- * Show instances (for Updates published to event bus)

deriving stock instance Show SetAllReposA

deriving stock instance Show AddNewRepoA

deriving stock instance Show DeleteRepoByNameA

deriving stock instance Show SetRepoA

deriving stock instance Show SetRepoBranchesA

deriving stock instance Show StoreCommitA

deriving stock instance Show AddNewJobA

deriving stock instance Show JobUpdateStatusA

deriving stock instance Show MarkUnfinishedJobsAsStaleA
