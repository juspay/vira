module Vira.CI.AutoBuild.Type (
  AutoBuildNewBranches (..),
) where

newtype AutoBuildNewBranches = AutoBuildNewBranches Bool
  deriving stock (Show, Eq)
