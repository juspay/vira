-- | Core types and utilities for ToolsPage
module Vira.Page.ToolsPage.Core (
  Tool (..),
  ToolDisplay (..),
) where

-- | Tool definition
data Tool = Tool
  { name :: Text
  , description :: Text
  , url :: Text
  , binPaths :: NonEmpty Text
  }

-- | Tool display styling
data ToolDisplay = ToolDisplay
  { initial :: Text
  , bgClass :: Text
  , textClass :: Text
  }
