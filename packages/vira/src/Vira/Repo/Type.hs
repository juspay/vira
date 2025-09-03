module Vira.Repo.Type where

import Vira.State.Type (AtticSettings, CachixSettings)

newtype RepoSettings = RepoSettings
  { stages :: [([Condition], Stage)]
  -- ^ All stages in a `Task` with their run conditions
  }
  deriving stock (Show)

-- | User-configurable stage in a `Task`
data Stage
  = AtticLogin AtticSettings
  | Build OmCiConfig
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
    BranchMatches Text
  deriving stock (Show)
