{-# LANGUAGE DuplicateRecordFields #-}

-- | Public interface for running Vira CI pipelines
module Vira.CI.Pipeline (
  -- * Running pipelines
  runPipeline,

  -- * Pipeline environment constructors
  pipelineEnvFromRemote,
  pipelineEnvFromCLI,

  -- * Error types
  PipelineError (..),
) where

import Vira.CI.Error (PipelineError (..))
import Vira.CI.Pipeline.Effect (pipelineEnvFromCLI, pipelineEnvFromRemote)
import Vira.CI.Pipeline.Implementation (runPipeline)
