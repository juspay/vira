module Vira.CI.Environment.Type where

import Vira.State.Type (AtticSettings, Branch, CachixSettings, Repo)

-- | The full context in which the CI pipeline is executed.
data ViraEnvironment = ViraEnvironment
  { repo :: Repo
  , branch :: Branch
  , cachixSettings :: Maybe CachixSettings
  , atticSettings :: Maybe AtticSettings
  }
