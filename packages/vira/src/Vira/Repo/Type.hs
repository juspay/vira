module Vira.Repo.Type where

import Data.Default (Default (def))
import System.FilePattern (FilePattern)
import Vira.State.Type (AtticSettings, CachixSettings)

newtype RepoSettings = RepoSettings
  { stages :: [(StageSettings, Stage)]
  -- ^ All stages in a `Task` with their run conditions
  }
  deriving stock (Show)

-- | Settings for when to run a stage
newtype StageSettings = StageSettings
  { if_ :: [Condition]
  -- ^ Enabled only if all of these conditions are met
  }
  deriving stock (Show)

-- By default, there are no conditions. All stages run.
instance Default StageSettings where
  def = StageSettings {if_ = []}

-- | User-configurable stage in a `Task`
data Stage
  = Build OmCiConfig
  | AtticPush AtticSettings
  | CachixPush CachixSettings
  deriving stock (Show)

-- | Settings for the build `Stage`
newtype OmCiConfig = OmCiConfig
  { extraArgs :: [Text]
  -- ^ extra CLI arguments to the `om ci run` command
  }
  deriving stock (Show)

-- | Condition for when to run a `Stage`
newtype Condition
  = -- | Whether the branch name of the current checkout matches the given pattern
    BranchMatches GlobPattern
  deriving stock (Show)

-- | Glob pattern for arbitrary strings; `FilePattern` is syntactically equivalent, so we use it.
type GlobPattern = FilePattern
