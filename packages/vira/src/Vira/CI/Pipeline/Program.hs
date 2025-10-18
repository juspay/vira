{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Program where

import Colog (Severity (..))
import Effectful
import Effectful.Error.Static (Error)
import Shower qualified
import Vira.CI.Error (PipelineError (..))
import Vira.CI.Pipeline.Effect

{- | Local pipeline program (for CLI - no clone)
Runs in working directory (handler sets cwd)
-}
pipelineLocalProgram ::
  (PipelineLocal :> es, PipelineLog :> es, Error PipelineError :> es) =>
  Eff es ()
pipelineLocalProgram = do
  logPipeline Info "Starting pipeline execution"

  -- Step 1: Load configuration
  pipeline <- loadConfig
  logPipeline Info $ toText $ "Pipeline configuration:\n" <> Shower.shower pipeline
  logPipeline Info "Loaded pipeline configuration"

  -- Step 2: Build
  buildResults <- build pipeline
  logPipeline Info $ "Built " <> show (length buildResults.results) <> " flakes"

  -- Step 3: Cache using build results
  cache pipeline buildResults
  logPipeline Info "Cache push completed"

  -- Step 4: Signoff
  signoff pipeline
  logPipeline Info "Pipeline completed successfully"

{- | Remote pipeline program (for web/CI - with clone)
Clones repository first, then runs local pipeline
-}
pipelineRemoteProgram ::
  (PipelineRemote :> es, PipelineLog :> es, Error PipelineError :> es) =>
  Eff es ()
pipelineRemoteProgram = do
  logPipeline Info "Starting pipeline with clone"

  -- Step 1: Clone repository
  cloneResults <- clone
  logPipeline Info $ "Repository cloned to " <> toText cloneResults.repoDir

  -- Step 2-5: Run local pipeline in the cloned directory
  runLocalPipeline cloneResults pipelineLocalProgram
