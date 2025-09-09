module Vira.Repo.Type where

import Data.Default (Default (def))
import System.FilePattern (FilePattern)

newtype RepoSettings = RepoSettings
  { stages :: Stages
  }
  deriving stock (Show)

-- | Wraps the settings for a stage with its run conditions
data StageSettings s = StageSettings
  { if_ :: [Condition]
  -- ^ Enabled only if all of these conditions are met
  , settings :: s
  }
  deriving stock (Show)

-- By default, there are no conditions. All stages run.
instance (Default s) => Default (StageSettings s) where
  def = StageSettings {if_ = [], settings = def}

-- | Configurable stages with their run conditions
newtype Stages = Stages
  { build :: StageSettings OmCiConfig
  -- ^ Settings for the build stage with its run conditions
  }
  deriving stock (Show)

instance Default Stages where
  def = Stages def

-- | Settings for the build stage
newtype OmCiConfig = OmCiConfig
  { extraArgs :: [Text]
  -- ^ extra CLI arguments to the `om ci run` command
  }
  deriving stock (Show)

instance Default OmCiConfig where
  def = OmCiConfig {extraArgs = []}

-- | Condition for when to run a stage
newtype Condition
  = -- | Whether the branch name of the current checkout matches the given pattern
    BranchMatches GlobPattern
  deriving stock (Show)

-- | Glob pattern for arbitrary strings; `FilePattern` is syntactically equivalent, so we use it.
type GlobPattern = FilePattern
