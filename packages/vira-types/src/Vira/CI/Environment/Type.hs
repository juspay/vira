{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Environment.Type (
  ViraEnvironment (..),
  projectDir,
) where

import System.FilePath ((</>))
import Vira.State.Type (AtticSettings, Branch, CachixSettings, Repo)

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
