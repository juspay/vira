{-# LANGUAGE DuplicateRecordFields #-}

module Vira.CI.AutoBuild.Type (
  AutoBuildNewBranches (..),
  AutoBuildSettings (..),
) where

newtype AutoBuildNewBranches = AutoBuildNewBranches Bool
  deriving stock (Show, Eq)

-- | Auto-build daemon settings
newtype AutoBuildSettings = AutoBuildSettings
  { autoBuildNewBranches :: AutoBuildNewBranches
  -- ^ Whether to automatically build new branches
  }
