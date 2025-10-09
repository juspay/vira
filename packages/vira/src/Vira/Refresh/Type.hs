{-# LANGUAGE TemplateHaskell #-}

module Vira.Refresh.Type where

import Data.Data (Data)
import Data.SafeCopy
import Data.Time (NominalDiffTime)
import Effectful.Git (RepoName)
import Prelude

-- | Configuration for the refresh daemon
data RefreshConfig = RefreshConfig
  { refreshInterval :: NominalDiffTime
  -- ^ How often to refresh repositories (in seconds)
  , enabled :: Bool
  -- ^ Whether auto-refresh is enabled
  }
  deriving stock (Show, Eq)

-- | Status of a repository refresh operation
data RefreshStatus
  = -- | Refresh completed successfully
    RefreshSuccess
  | -- | Refresh failed with an error message
    RefreshFailure Text
  | -- | Refresh is queued or in progress
    RefreshPending
  deriving stock (Generic, Typeable, Data, Eq, Show, Ord)

$(deriveSafeCopy 0 'base ''RefreshStatus)

-- | Commands that can be sent to the refresh daemon
data RefreshCommand
  = -- | Refresh a specific repository
    RefreshRepo RepoName RefreshPriority
  | -- | Refresh all repositories (periodic auto-refresh)
    RefreshAll
  deriving stock (Show, Eq)

-- | Priority level for refresh operations
data RefreshPriority
  = -- | Manual refresh triggered by user
    Manual
  | -- | Automatic refresh from timer
    Automatic
  deriving stock (Show, Eq, Ord)
