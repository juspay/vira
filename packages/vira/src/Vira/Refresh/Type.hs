{-# LANGUAGE TemplateHaskell #-}

module Vira.Refresh.Type where

import Data.Data (Data)
import Data.SafeCopy
import Data.Time (NominalDiffTime)
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
  = RefreshSuccess
  | RefreshFailure Text
  | RefreshPending
  deriving stock (Generic, Typeable, Data, Eq, Show, Ord)

$(deriveSafeCopy 0 'base ''RefreshStatus)
