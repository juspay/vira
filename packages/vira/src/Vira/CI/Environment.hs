{-# LANGUAGE RecordWildCards #-}

module Vira.CI.Environment (module Vira.CI.Environment.Type, environmentFor) where

import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Reader
import Vira.App qualified as App
import Vira.App.Stack (AppState)
import Vira.CI.Environment.Type
import Vira.State.Acid qualified as St
import Vira.State.Type (Branch, Repo)

-- | Construct the 'ViraEnvironment' for a given repository and branch.
environmentFor ::
  ( Reader.Reader AppState :> es
  , IOE :> es
  ) =>
  Repo ->
  Branch ->
  FilePath ->
  Eff es ViraEnvironment
environmentFor repo branch workspacePath = do
  cachixSettings <- App.query St.GetCachixSettingsA
  atticSettings <- App.query St.GetAtticSettingsA
  pure $ ViraEnvironment {..}
