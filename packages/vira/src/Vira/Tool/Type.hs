-- | Tool type definitions (split from Tool.hs to avoid circular dependencies)
module Vira.Tool.Type (
  Tools (..),
  ToolData (..),
  ToolError (..),
) where

import Attic.Config (AtticConfig)
import GH.Auth.Status (AuthStatus)
import TOML (TOMLError)

-- | All tools with their metadata and runtime info
data Tools = Tools
  { attic :: (ToolData, Either TOMLError (Maybe AtticConfig))
  , github :: (ToolData, AuthStatus)
  , omnix :: (ToolData, ())
  , git :: (ToolData, ())
  , cachix :: (ToolData, ())
  }
  deriving stock (Show)

-- | Tool metadata (name, description, binaries)
data ToolData = ToolData
  { name :: Text
  , description :: Text
  , url :: Text
  , binPaths :: NonEmpty Text
  }
  deriving stock (Show)

-- | Tool-related errors
newtype ToolError = ToolError Text
  deriving stock (Show)
