{-# LANGUAGE DuplicateRecordFields #-}

-- | Tools type definition
module Vira.Tool.Type.Tools (
  Tools (..),
) where

import Attic.Config (AtticConfig)
import GH.Auth.Status (AuthStatus)
import Vira.Tool.Tools.Attic (ConfigError)
import Vira.Tool.Type.ToolData (ToolData)

-- | All tools with their metadata and runtime status
data Tools = Tools
  { attic :: ToolData (Either ConfigError AtticConfig)
  , github :: ToolData AuthStatus
  , omnix :: ToolData ()
  , git :: ToolData ()
  , cachix :: ToolData ()
  }
  deriving stock (Show)
