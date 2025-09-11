module Vira.CI.Environment where

import Vira.State.Core qualified as St
import Vira.State.Type (AtticSettings, CachixSettings)

-- | Environment containing all necessary configuration for CI pipeline execution
data ViraEnvironment = ViraEnvironment
  { repo :: St.Repo
  , branch :: St.Branch
  , cachixSettings :: Maybe CachixSettings
  , atticSettings :: Maybe AtticSettings
  }
