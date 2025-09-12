{-# LANGUAGE RecordWildCards #-}

module Vira.CI.Environment where

import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Reader
import Vira.App qualified as App
import Vira.App.Stack (AppState)
import Vira.State.Acid qualified as St
import Vira.State.Core qualified as St
import Vira.State.Type (AtticSettings, CachixSettings)

-- | The full context in which the CI pipeline is executed.
data ViraEnvironment = ViraEnvironment
  { repo :: St.Repo
  , branch :: St.Branch
  , cachixSettings :: Maybe CachixSettings
  , atticSettings :: Maybe AtticSettings
  }

-- | Construct the 'ViraEnvironment' for a given repository and branch.
environmentFor ::
  ( Reader.Reader AppState :> es
  , IOE :> es
  ) =>
  St.Repo ->
  St.Branch ->
  Eff es ViraEnvironment
environmentFor repo branch = do
  cachixSettings <- App.query St.GetCachixSettingsA
  atticSettings <- App.query St.GetAtticSettingsA
  pure $ ViraEnvironment {..}
