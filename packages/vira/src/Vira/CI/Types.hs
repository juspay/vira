{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Types (
  ViraPipeline (..),
  BuildStage (..),
  AtticStage (..),
  CachixStage (..),
  SignoffStage (..),
  mergePipelines,
) where

import Data.Aeson (FromJSON (..))
import Optics.TH

-- | Pipeline with optional stages (Nothing = disabled, Just = enabled)
data ViraPipeline = ViraPipeline
  { build :: Maybe BuildStage
  , attic :: Maybe AtticStage
  , cachix :: Maybe CachixStage
  , signoff :: Maybe SignoffStage
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

-- | Build stage with optional settings
newtype BuildStage = BuildStage
  { overrideInputs :: Maybe (Map Text Text) -- Nothing = use defaults
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

-- | Other stages become simple unit types
data AtticStage = AtticStage
  deriving stock (Generic, Show, Eq)

data CachixStage = CachixStage
  deriving stock (Generic, Show, Eq)

data SignoffStage = SignoffStage
  deriving stock (Generic, Show, Eq)

instance FromJSON AtticStage where
  parseJSON _ = pure AtticStage

instance FromJSON CachixStage where
  parseJSON _ = pure CachixStage

instance FromJSON SignoffStage where
  parseJSON _ = pure SignoffStage

-- | Empty pipeline (all stages disabled)
instance Monoid ViraPipeline where
  mempty = ViraPipeline Nothing Nothing Nothing Nothing

instance Semigroup ViraPipeline where
  (<>) = mergePipelines

{- | Merge two pipelines, with the second overriding the first
Only non-Nothing fields in override replace the base
-}
mergePipelines :: ViraPipeline -> ViraPipeline -> ViraPipeline
mergePipelines
  (ViraPipeline baseBuild baseAttic baseCachix baseSignoff)
  (ViraPipeline overrideBuild overrideAttic overrideCachix overrideSignoff) =
    ViraPipeline
      { build = overrideBuild `orElse` baseBuild
      , attic = overrideAttic `orElse` baseAttic
      , cachix = overrideCachix `orElse` baseCachix
      , signoff = overrideSignoff `orElse` baseSignoff
      }
    where
      orElse :: Maybe a -> Maybe a -> Maybe a
      orElse Nothing base = base
      orElse override _ = override

makeLenses ''ViraPipeline
makeLenses ''BuildStage
makeLenses ''SignoffStage
