{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline.Effect where

import Colog (Severity)
import Effectful
import Effectful.TH
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

{- | PipelineLocal Effect - core pipeline operations without clone
Used for CLI execution in current directory
-}
data PipelineLocal :: Effect where
  -- | Load vira.hs configuration from repository
  LoadConfig :: FilePath -> PipelineLocal m ViraPipeline
  -- | Build flakes and return result paths
  Build :: FilePath -> ViraPipeline -> PipelineLocal m BuildResults
  -- | Push build results to cache (throws error on failure)
  Cache :: ViraPipeline -> BuildResults -> PipelineLocal m ()
  -- | Create GitHub commit status (throws error on failure)
  Signoff :: FilePath -> ViraPipeline -> PipelineLocal m ()
  -- | Log a message (convenience)
  LogPipeline :: Severity -> Text -> PipelineLocal m ()

-- Generate boilerplate for the effect
makeEffect ''PipelineLocal

-- | Pipeline Effect - extends PipelineLocal with clone for web/CI
data Pipeline :: Effect where
  -- | Clone repository and return cloned directory
  Clone :: Pipeline m CloneResults

-- Generate boilerplate for the effect
makeEffect ''Pipeline

-- | Environment for local pipeline (CLI - no clone needed)
data PipelineLocalEnv = PipelineLocalEnv
  { baseDir :: FilePath
  -- ^ Base directory to execute from (repo dir for CLI, workspace for web)
  , outputLog :: Maybe FilePath
  -- ^ Optional output log file
  , tools :: Tools
  -- ^ Available CI tools
  , viraContext :: ViraContext
  -- ^ Vira context (branch, dirty flag)
  }
  deriving stock (Generic)

-- | Context needed by the full pipeline handler (with clone)
data PipelineEnv = PipelineEnv
  { localEnv :: PipelineLocalEnv
  -- ^ Environment for local operations (reused from PipelineLocal)
  , viraEnv :: ViraEnvironment
  -- ^ Full environment with repo/branch info (only needed for Clone)
  }
  deriving stock (Generic)
