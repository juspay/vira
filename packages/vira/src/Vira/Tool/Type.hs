{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tool type definitions (split from Tool.hs to avoid circular dependencies)
module Vira.Tool.Type (
  Tool (..),
  ToolData (ToolData, name, description, url, binPaths, info),
  ToolError (..),
) where

import Attic.Config (AtticConfig)
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Some (Some (..))
import GH.Auth.Status (AuthStatus)
import TOML (TOMLError)

-- | GADT for tool keys with their info types inlined
data Tool info where
  Attic :: Tool (Either TOMLError (Maybe AtticConfig))
  GitHub :: Tool AuthStatus
  Omnix :: Tool ()
  Git :: Tool ()
  Cachix :: Tool ()

$(deriveGEq ''Tool)
$(deriveGCompare ''Tool)
$(deriveGShow ''Tool)

-- | Tool data combining metadata and runtime info
data ToolData info = ToolData
  { name :: Text
  , description :: Text
  , url :: Text
  , binPaths :: NonEmpty Text
  , info :: info
  }

-- | Tool-related errors with the tool that caused them
data ToolError = ToolError (Some Tool) Text

deriving stock instance Show ToolError
