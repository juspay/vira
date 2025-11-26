{-# LANGUAGE DuplicateRecordFields #-}

-- | Tools type definition
module Vira.Environment.Tool.Type.Tools (
  Tools (..),
) where

import Attic.Config (AtticConfig)
import BB.Auth.Status qualified as BB
import GH.Auth.Status (AuthStatus)
import Vira.Environment.Tool.Tools.Attic (ConfigError)
import Vira.Environment.Tool.Tools.Nix (NixStatus)
import Vira.Environment.Tool.Type.ToolData (ToolData)

-- | All tools with their metadata and runtime status
data Tools = Tools
  { attic :: ToolData (Either ConfigError AtticConfig)
  -- ^ Attic binary cache tool with 'Attic.Config.AtticConfig' status
  , bitbucket :: ToolData BB.AuthStatus
  -- ^ Bitbucket CLI tool with 'BB.AuthStatus'
  , github :: ToolData AuthStatus
  -- ^ GitHub CLI tool with 'GH.Auth.Status.AuthStatus'
  , git :: ToolData ()
  -- ^ Git version control tool
  , cachix :: ToolData ()
  -- ^ Cachix binary cache tool
  , nix :: ToolData (Either Text NixStatus)
  -- ^ Nix package manager with 'Vira.Environment.Tool.Tools.Nix.NixStatus'
  }
  deriving stock (Show)
