{-# LANGUAGE DuplicateRecordFields #-}

-- | Public interface for running Vira CI pipelines
module Vira.CI.Pipeline (
  runWebPipeline,
  runCLIPipeline,
  defaultPipeline,
  PipelineError (..),
) where

import Vira.CI.Error (PipelineError (..))
import Vira.CI.Pipeline.Implementation (defaultPipeline, runCLIPipeline, runWebPipeline)
