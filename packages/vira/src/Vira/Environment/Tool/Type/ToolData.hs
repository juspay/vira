{-# LANGUAGE DuplicateRecordFields #-}

-- | ToolData type definition
module Vira.Environment.Tool.Type.ToolData (
  ToolData (..),
) where

-- | Tool metadata combined with runtime status
data ToolData statusType = ToolData
  { name :: Text
  -- ^ Human-readable tool name
  , url :: Text
  -- ^ URL to the tool's homepage or documentation
  , binPaths :: NonEmpty Text
  -- ^ File paths to the tool's executables
  , status :: statusType
  -- ^ Tool-specific runtime status information (e.g., 'GH.Auth.Status.AuthStatus', config, etc.)
  }
  deriving stock (Show)
