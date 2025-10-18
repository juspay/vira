{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline.Effect where

import Colog (Severity)
import Effectful
import Effectful.TH
import System.Exit (ExitCode)
import Vira.CI.Context (ViraContext)
import Vira.CI.Environment (ViraEnvironment)
import Vira.CI.Pipeline.Type (ViraPipeline)
import Vira.Tool.Type.Tools (Tools)

-- | Results from clone step
data CloneResults = CloneResults
  { repoDir :: FilePath
  -- ^ Path to cloned repository
  , commitId :: Text
  -- ^ The commit that was checked out
  }
  deriving stock (Show, Eq, Generic)

-- | Results from a single flake build
data BuildResult = BuildResult
  { flakePath :: FilePath
  -- ^ Original flake path (relative to repo root)
  , resultSymlink :: FilePath
  -- ^ Relative path to result symlink (relative to repo root)
  }
  deriving stock (Show, Eq, Generic)

-- | Results from build step that inform subsequent steps
newtype BuildResults = BuildResults
  { results :: NonEmpty BuildResult
  -- ^ Build results for each flake
  }
  deriving stock (Show, Eq, Generic)

-- | Get all result paths (for cache pushing)
resultPaths :: BuildResults -> NonEmpty FilePath
resultPaths = fmap (.resultSymlink) . (.results)

-- | Pipeline Effect - defines operations available during pipeline execution
data Pipeline :: Effect where
  -- | Clone repository and return cloned directory
  Clone :: Pipeline m CloneResults
  -- | Load vira.hs configuration from repository
  LoadConfig :: FilePath -> Pipeline m ViraPipeline
  -- | Build flakes and return result paths
  Build :: FilePath -> ViraPipeline -> Pipeline m BuildResults
  -- | Push build results to cache
  Cache :: ViraPipeline -> BuildResults -> Pipeline m ExitCode
  -- | Create GitHub commit status
  Signoff :: ViraPipeline -> Pipeline m ExitCode
  -- | Log a message (convenience)
  LogPipeline :: Severity -> Text -> Pipeline m ()

-- Generate boilerplate for the effect
makeEffect ''Pipeline

-- | Context needed by the pipeline handler
data PipelineEnv = PipelineEnv
  { workspaceDir :: FilePath
  -- ^ Workspace directory (parent of repo)
  , outputLog :: Maybe FilePath
  -- ^ Optional output log file
  , tools :: Tools
  -- ^ Available CI tools
  , viraContext :: ViraContext
  -- ^ Vira context (branch, dirty flag)
  , viraEnv :: ViraEnvironment
  -- ^ Full environment with repo/branch info
  }
  deriving stock (Generic)
