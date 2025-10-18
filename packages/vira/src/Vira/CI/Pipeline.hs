{-# LANGUAGE DuplicateRecordFields #-}

-- | Public interface for running Vira CI pipelines
module Vira.CI.Pipeline (
  -- * Running pipelines
  runPipelineRemote,
  runPipelineLocal,

  -- * Pipeline environment and error types
  pipelineRemoteEnvFrom,
  pipelineLocalEnvFrom,
  PipelineError (..),
) where

import Vira.CI.Error (PipelineError (..))
import Vira.CI.Pipeline.Effect (pipelineLocalEnvFrom, pipelineRemoteEnvFrom)
import Vira.CI.Pipeline.Implementation (runPipelineLocal, runPipelineRemote)
