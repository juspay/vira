{-# LANGUAGE DuplicateRecordFields #-}

-- | Public interface for running Vira CI pipelines
module Vira.CI.Pipeline (
  -- * Running pipelines
  runPipeline,

  -- * Pipeline environment constructors
  pipelineEnvFromRemote,
  pipelineEnvFromCLI,

  -- * Logging
  logPipeline',
  workspaceContextKeys,

  -- * Error types
  PipelineError (..),
) where

import Vira.CI.Error (PipelineError (..))
import Vira.CI.Pipeline.Effect (logPipeline', pipelineEnvFromCLI, pipelineEnvFromRemote, workspaceContextKeys)
import Vira.CI.Pipeline.Implementation (runPipeline)
