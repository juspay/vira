-- | Auto-refresh system for Git repositories
module Vira.Refresh (
  -- * State management
  RefreshState,
  newRefreshState,
  getRefreshStatus,

  -- * Types (for external use)
  RefreshPriority (..),
) where

import Vira.Refresh.Type (RefreshPriority (..), RefreshState (..), getRefreshStatus, newRefreshState)
