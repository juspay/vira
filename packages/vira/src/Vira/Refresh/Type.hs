{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Types for the refresh system (internal)
module Vira.Refresh.Type (
  -- * State
  RefreshState (..),
  newRefreshState,
  getRefreshStatus,

  -- * Status
  RefreshStatus (..),

  -- * Priority
  RefreshPriority (..),
) where

import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, UTCTime)
import Effectful.Git (RepoName)

-- * State

-- | Opaque state container for the refresh system
newtype RefreshState = RefreshState
  { statusMap :: TVar (Map RepoName RefreshStatus)
  }
  deriving stock (Generic)

-- | Create a new refresh state
newRefreshState :: IO RefreshState
newRefreshState = do
  statusMap <- newTVarIO Map.empty
  pure RefreshState {statusMap}

-- | Get the current refresh status for a repository
getRefreshStatus :: RefreshState -> RepoName -> IO RefreshStatus
getRefreshStatus st repo = do
  statusMap <- readTVarIO st.statusMap
  pure $ Map.findWithDefault NeverRefreshed repo statusMap

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
  | -- | Successfully refreshed
    Success
      { completedAt :: UTCTime
      , duration :: NominalDiffTime
      }
  | -- | Refresh failed
    Failed
      { completedAt :: UTCTime
      , duration :: NominalDiffTime
      , errorMsg :: Text
      }
  deriving stock (Eq, Show, Generic)

-- * Priority

-- | Priority for refresh requests
data RefreshPriority
  = -- | High priority (manual user request)
    Now
  | -- | Normal priority (scheduled background refresh)
    Normal
  deriving stock (Eq, Ord, Show, Generic)
