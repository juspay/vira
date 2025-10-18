{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Vira.CI.Pipeline.Effect where

import Colog (Severity)
import Effectful
import Effectful.Git.Types (CommitID)
import Effectful.TH
import Vira.CI.Context (ViraContext)
import Vira.CI.Environment (ViraEnvironment)
import Vira.CI.Pipeline.Type (ViraPipeline)
import Vira.Tool.Type.Tools (Tools)

-- | Results from clone step
data CloneResults = CloneResults
  { repoDir :: FilePath
  -- ^ Path to cloned repository
  , commitId :: CommitID
  -- ^ The commit that was checked out
  }
  deriving stock (Show, Eq, Generic)

-- | Results from build step that inform subsequent steps
newtype BuildResults = BuildResults
  { results :: NonEmpty FilePath
  -- ^ Relative paths to result symlinks (relative to repo root)
  }
  deriving stock (Show, Eq, Generic)

{- | PipelineLocal Effect - core pipeline operations without clone
Used for CLI execution in current directory
-}
data PipelineLocal :: Effect where
  -- | Load vira.hs configuration from repository (runs in working dir)
  LoadConfig :: PipelineLocal m ViraPipeline
  -- | Build flakes and return result paths (runs in working dir)
  Build :: ViraPipeline -> PipelineLocal m BuildResults
  -- | Push build results to cache (throws error on failure)
  Cache :: ViraPipeline -> BuildResults -> PipelineLocal m ()
  -- | Create GitHub commit status (throws error on failure, runs in working dir)
  Signoff :: ViraPipeline -> PipelineLocal m ()
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
  { outputLog :: Maybe FilePath
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
  -- ^ Full environment with repo/branch info (workspacePath in here)
  }
  deriving stock (Generic)
