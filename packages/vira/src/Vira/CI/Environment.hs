{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.CI.Environment (
  ViraEnvironment (..),
  projectDir,
  environmentFor,
  viraContext,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Reader
import System.FilePath ((</>))
import Vira.App qualified as App
import Vira.App.Stack (AppState)
import Vira.CI.Context (ViraContext (..))
import Vira.State.Acid qualified as St
import Vira.State.Type (AtticSettings, Branch (..), CachixSettings, Repo)
import Vira.Tool.Core qualified as Tool
import Vira.Tool.Type (Tools)

-- | The full context in which the CI pipeline is executed.
data ViraEnvironment = ViraEnvironment
  { repo :: Repo
  , branch :: Branch
  , cachixSettings :: Maybe CachixSettings
  , atticSettings :: Maybe AtticSettings
  , tools :: Tools
  -- ^ All tools with their runtime info (configs, auth status, etc.)
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
  -- Refresh all tools with their latest runtime info
  tools <- Tool.refreshTools
  pure $ ViraEnvironment {..}

-- | Extract ViraContext from ViraEnvironment
viraContext :: ViraEnvironment -> ViraContext
viraContext env =
  let envBranch = env.branch
   in ViraContext
        { branch = branchName envBranch
        , commit = headCommit envBranch
        }
