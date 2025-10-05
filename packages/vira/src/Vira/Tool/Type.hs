-- | Tool type definitions (split from Tool.hs to avoid circular dependencies)
module Vira.Tool.Type (
  Tools (..),
  ToolData (..),
  ToolError (..),
) where

import Attic.Config (AtticConfig)
import GH.Auth.Status (AuthStatus)
import TOML (TOMLError)

-- | Tool metadata combined with runtime status
data ToolData statusType = ToolData
  { name :: Text
  -- ^ Human-readable tool name
  , description :: Text
  -- ^ Brief description of what the tool does
  , url :: Text
  -- ^ URL to the tool's homepage or documentation
  , binPaths :: NonEmpty Text
  -- ^ File paths to the tool's executables
  , status :: statusType
  -- ^ Tool-specific runtime status information (auth status, config, etc.)
  }
  deriving stock (Show)

-- | All tools with their metadata and runtime status
data Tools = Tools
  { attic :: ToolData (Either TOMLError (Maybe AtticConfig))
  , github :: ToolData AuthStatus
  , omnix :: ToolData ()
  , git :: ToolData ()
  , cachix :: ToolData ()
  }
  deriving stock (Show)

-- | Tool-related errors
newtype ToolError = ToolError Text
  deriving stock (Show)
