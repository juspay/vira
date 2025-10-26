{-# LANGUAGE DuplicateRecordFields #-}

-- | Nix configuration and remote builders
module System.Nix.Config (
  -- * Builders
  Builders (..),
  RemoteBuilder (..),
  resolveBuilders,

  -- * Configuration
  NixConfig (..),
  NixConfigField (..),
  nixConfigShow,
) where

import System.Nix.Config.Builders (Builders (..), RemoteBuilder (..))
import System.Nix.Config.Core (NixConfig (..), NixConfigField (..), nixConfigShow, resolveBuilders)
