{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types for the refresh system (internal)
module Vira.Refresh.Type (
  -- * State
  RefreshState (..),
  newRefreshState,

  -- * Status
  RefreshStatus (..),
  RefreshResult (..),
  RefreshOutcome (..),

  -- * Priority
  RefreshPriority (..),
) where

import Control.Concurrent.Async (Async)
import Data.Data (Data)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (NominalDiffTime, UTCTime)
import Effectful.Git (RepoName)

-- * State

-- | Opaque state container for the refresh system
data RefreshState = RefreshState
  { statusMap :: TVar (Map RepoName RefreshStatus)
  -- ^ Map from 'RepoName' to current 'RefreshStatus'
  , daemonHandle :: TVar (Maybe (Async Void))
  -- ^ Handle to the refresh daemon worker thread
  }
  deriving stock (Generic)

-- | Create a new 'RefreshState' (starts empty, populated by 'Vira.Refresh.Daemon.startRefreshDaemon' on startup)
newRefreshState :: IO RefreshState
newRefreshState = do
  statusMap <- newTVarIO mempty
  daemonHandle <- newTVarIO Nothing
  pure RefreshState {..}

-- * Status

-- | Status of a repository refresh operation
data RefreshStatus
  = -- | Never been refreshed
    NeverRefreshed
  | -- | Queued for refresh
    Pending
      { queuedAt :: UTCTime
      , priority :: RefreshPriority
      }
  | -- | Currently being refreshed
    InProgress
      { startedAt :: UTCTime
      }
  | -- | Refresh completed (success or failure)
    Completed RefreshResult
  deriving stock (Eq, Show, Generic)

-- | Result of a completed refresh operation (stored in acid-state and 'RefreshState')
data RefreshResult = RefreshResult
  { completedAt :: UTCTime
  , duration :: NominalDiffTime
  , outcome :: RefreshOutcome
  }
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

-- | Outcome of a refresh operation ('Success' or 'Failure')
data RefreshOutcome
  = -- | Refresh succeeded
    Success
  | -- | Refresh failed with error message
    Failure Text
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

-- * Priority

-- | Priority for refresh requests
data RefreshPriority
  = -- | High priority (manual user request)
    Now
  | -- | Normal priority (scheduled background refresh)
    Normal
  deriving stock (Eq, Ord, Show, Generic)

-- * SafeCopy instances

$(deriveSafeCopy 0 'base ''RefreshOutcome)
$(deriveSafeCopy 0 'base ''RefreshResult)
