{-# LANGUAGE DuplicateRecordFields #-}

-- | Tools type definition
module Vira.Environment.Tool.Type.Tools (
  Tools (..),
) where

import Attic.Config (AtticConfig)
import GH.Auth.Status (AuthStatus)
import Vira.Environment.Tool.Tools.Attic (ConfigError)
import Vira.Environment.Tool.Tools.Nix (NixStatus)
import Vira.Environment.Tool.Type.ToolData (ToolData)

-- | All tools with their metadata and runtime status
data Tools = Tools
  { attic :: ToolData (Either ConfigError AtticConfig)
  , github :: ToolData AuthStatus
  , git :: ToolData ()
  , cachix :: ToolData ()
  , nix :: ToolData (Either Text NixStatus)
  }
  deriving stock (Show)
