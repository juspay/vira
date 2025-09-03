module Vira.Repo.Type where

import Vira.State.Type (AtticSettings, CachixSettings)

newtype RepoSettings = RepoSettings
  { stages :: [Stage]
  -- ^ All stages in a `Task`
  }
  deriving stock (Show)

-- | `Action` with conditions
data Stage = Stage
  { conditions :: [ActionCondition]
  , action :: Action
  }
  deriving stock (Show)

-- | User-configurable action in a `Task`
data Action
  = AtticLogin AtticSettings
  | Build BuildSettings
  | AtticPush AtticSettings
  | CachixPush CachixSettings
  deriving stock (Show)

-- | Settings for the build `Action`
newtype BuildSettings = BuildSettings
  { extraArgs :: [Text]
  -- ^ extra CLI arguments to the build command
  }
  deriving stock (Show)

-- | Condition for when to run an `Action`
newtype ActionCondition
  = -- | Whether the branch name of the current checkout matches the given pattern
    BranchMatches Text
  deriving stock (Show)
