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
Runs in current directory
-}
runPipelineProgramLocal ::
  (PipelineLocal :> es, Error PipelineError :> es) =>
  -- | Repository directory (current directory for CLI)
  FilePath ->
  Eff es ()
runPipelineProgramLocal repoDir = do
  logPipeline Info "Starting pipeline execution"

  -- Step 1: Load configuration
  pipeline <- loadConfig repoDir
  logPipeline Info $ toText $ "Pipeline configuration:\n" <> Shower.shower pipeline
  logPipeline Info "Loaded pipeline configuration"

  -- Step 2: Build using repository path
  buildResults <- build repoDir pipeline
  logPipeline Info $ "Built " <> show (length $ resultPaths buildResults) <> " flakes"

  -- Step 3: Cache using build results
  cache pipeline buildResults
  logPipeline Info "Cache push completed"

  -- Step 4: Signoff
  signoff pipeline
  logPipeline Info "Pipeline completed successfully"

{- | Full pipeline program (for web/CI - with clone)
Clones repository first, then runs local pipeline
-}
runPipelineProgram ::
  (Pipeline :> es, PipelineLocal :> es, Error PipelineError :> es) =>
  Eff es ()
runPipelineProgram = do
  logPipeline Info "Starting pipeline with clone"

  -- Step 1: Clone repository
  cloneResults <- clone
  logPipeline Info $ "Cloned to " <> toText cloneResults.repoDir

  -- Step 2-5: Run local pipeline in cloned directory
  runPipelineProgramLocal cloneResults.repoDir
