{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Show instances for acid-state events (required for event bus).
-}
module Vira.State.AcidInstances () where

import Vira.State.Acid

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
