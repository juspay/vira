{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types for the auto-build system
module Vira.AutoBuild.Type (
  -- * State
  AutoBuildState (..),
  newAutoBuildState,

  -- * Build Request
  BuildRequest (..),
  BuildPriority (..),
) where

import Colog.Core (Severity)
import Control.Concurrent.Async (Async)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (UTCTime)
import Effectful.Git (BranchName, CommitID, RepoName)

-- * State

-- | Opaque state container for the auto-build system
data AutoBuildState = AutoBuildState
  { pendingBuilds :: TVar (Map (RepoName, BranchName) BuildRequest)
  -- ^ Pending build requests indexed by (repo, branch)
  , lastBuiltCommit :: TVar (Map (RepoName, BranchName) CommitID)
  -- ^ Tracks last built commit for each branch
  , maxConcurrentBuilds :: Natural
  -- ^ Maximum concurrent builds (0 = unlimited)
  , logLevel :: Severity
  -- ^ Minimum log level for builds
  , daemonHandle :: TVar (Maybe (Async Void))
  -- ^ Handle to the daemon thread
  }
  deriving stock (Generic)

-- | Create a new auto-build state
newAutoBuildState :: Natural -> Severity -> IO AutoBuildState
newAutoBuildState maxConcurrentBuilds logLevel = do
  pendingBuilds <- newTVarIO mempty
  lastBuiltCommit <- newTVarIO mempty
  daemonHandle <- newTVarIO Nothing
  pure AutoBuildState {..}

-- * Build Request

-- | A request to build a specific commit
data BuildRequest = BuildRequest
  { repo :: RepoName
  , branch :: BranchName
  , commit :: CommitID
  , priority :: BuildPriority
  , queuedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

-- | Priority for build requests
data BuildPriority
  = -- | Manual user request
    Manual
  | -- | Automatic trigger from branch update
    Auto
  deriving stock (Eq, Ord, Show, Generic)

-- * SafeCopy instances
$(deriveSafeCopy 0 'base ''BuildPriority)
