{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.CI.Environment (
  ViraEnvironment (..),
  projectDir,
  projectDirName,
  environmentFor,
  viraContext,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Reader
import System.FilePath ((</>))
import Vira.App.Stack (AppState)
import Vira.CI.Context (ViraContext (..))
import Vira.State.Type (Branch (..), Repo)
import Vira.Tool.Core qualified as Tool
import Vira.Tool.Type.Tools (Tools)

-- | The full context in which the CI pipeline is executed.
data ViraEnvironment = ViraEnvironment
  { repo :: Repo
  , branch :: Branch
  , tools :: Tools
  -- ^ All tools with their runtime info (configs, auth status, etc.)
  , workspacePath :: FilePath
  -- ^ Workspace directory path
  }

projectDirName :: FilePath
projectDirName = "project"

-- | Get the project directory path from the workspace
projectDir :: ViraEnvironment -> FilePath
projectDir env = env.workspacePath </> projectDirName

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
  -- Refresh all tools with their latest runtime info
  tools <- Tool.refreshTools
  pure $ ViraEnvironment {..}

-- | Extract ViraContext from ViraEnvironment
viraContext :: ViraEnvironment -> ViraContext
viraContext env =
  let envBranch = env.branch
   in ViraContext
        { branch = envBranch.branchName
        , commit = envBranch.headCommit
        }
