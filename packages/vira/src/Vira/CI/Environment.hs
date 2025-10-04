{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.CI.Environment (
  ViraEnvironment (..),
  projectDir,
  environmentFor,
  viraContext,
) where

import Attic.Config (AtticConfig)
import Attic.Config qualified
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Reader
import System.FilePath ((</>))
import TOML (TOMLError)
import Vira.App qualified as App
import Vira.App.Stack (AppState)
import Vira.CI.Context (ViraContext (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (AtticSettings, Branch (..), CachixSettings, Repo)

-- | The full context in which the CI pipeline is executed.
data ViraEnvironment = ViraEnvironment
  { repo :: Repo
  , branch :: Branch
  , cachixSettings :: Maybe CachixSettings
  , atticSettings :: Maybe AtticSettings
  , atticConfig :: Either TOMLError (Maybe AtticConfig)
  -- ^ Attic configuration from ~/.config/attic/config.toml (Left = parse error, Right Nothing = not configured)
  -- TODO: Use 'Tools' more generally instead of reading configs individually
  , workspacePath :: FilePath
  -- ^ Workspace directory path
  }

-- | Get the project directory path from the workspace
projectDir :: ViraEnvironment -> FilePath
projectDir env = env.workspacePath </> "project"

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
  -- Read attic config for cache stage validation
  atticConfig <- liftIO Attic.Config.readAtticConfig
  pure $ ViraEnvironment {..}

-- | Extract ViraContext from ViraEnvironment
viraContext :: ViraEnvironment -> ViraContext
viraContext env =
  let envBranch = env.branch
   in ViraContext
        { branch = branchName envBranch
        , commit = headCommit envBranch
        }
