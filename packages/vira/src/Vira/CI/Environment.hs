{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.CI.Environment (
  ViraEnvironment (..),
  projectDir,
  sharedCloneDir,
  environmentFor,
  viraContext,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Reader
import System.FilePath (takeDirectory, (</>))
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
  , workspacePath :: FilePath
  -- ^ Workspace directory path
  }

-- | Get the project directory path from the workspace
projectDir :: ViraEnvironment -> FilePath
projectDir env = env.workspacePath </> "project"

{- | Get the shared clone directory path from the workspace
Job workspace: baseWorkDir/repo/jobs/jobId
Shared clone:  baseWorkDir/repo/source
-}
sharedCloneDir :: ViraEnvironment -> FilePath
sharedCloneDir env =
  let jobWorkspace = env.workspacePath
      -- Remove "/jobs/jobId" to get the repo directory
      repoDir = takeDirectory $ takeDirectory jobWorkspace
   in repoDir </> "source"

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

-- | Extract ViraContext from ViraEnvironment
viraContext :: ViraEnvironment -> ViraContext
viraContext env =
  let envBranch = env.branch
   in ViraContext
        { branch = branchName envBranch
        , commit = headCommit envBranch
        }
