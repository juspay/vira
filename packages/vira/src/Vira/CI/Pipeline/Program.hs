{-# LANGUAGE DuplicateRecordFields #-}

module Vira.CI.Pipeline.Program where

import Colog (Severity (..))
import Effectful
import Effectful.Error.Static (Error)
import Shower qualified
import System.Exit (ExitCode (..))
import Vira.CI.Error (PipelineError (..))
import Vira.CI.Pipeline.Effect

{- | The main pipeline program - pure business logic!
This is completely separate from implementation details

NOTE: Clone step is currently handled outside this program in runPipeline.
Future refactoring could move it inside (as per original plan in issue #223)
-}
runPipelineProgram ::
  (Pipeline :> es, Error PipelineError :> es) =>
  -- | Repository directory (already cloned)
  FilePath ->
  Eff es ExitCode
runPipelineProgram repoDir = do
  logPipeline Info "Starting pipeline execution"

  -- Step 1: Load configuration
  pipeline <- loadConfig repoDir
  logPipeline Info $ toText $ "Pipeline configuration:\n" <> Shower.shower pipeline
  logPipeline Info "Loaded pipeline configuration"

  -- Step 2: Build using repository path
  buildResults <- build repoDir pipeline
  logPipeline Info $ "Built " <> show (length $ resultPaths buildResults) <> " flakes"

  -- Step 3: Cache using build results
  cacheExitCode <- cache pipeline buildResults
  case cacheExitCode of
    ExitSuccess -> logPipeline Info "Cache push completed"
    ExitFailure code -> do
      logPipeline Error $ "Cache push failed with code " <> show code
  -- Note: We don't throw here, we continue to signoff
  -- This matches current behavior where cache failures don't stop signoff

  -- Step 4: Signoff
  signoffExitCode <- signoff pipeline
  case signoffExitCode of
    ExitSuccess -> do
      logPipeline Info "Pipeline completed successfully"
      pure ExitSuccess
    ExitFailure code -> do
      logPipeline Error $ "Signoff failed with code " <> show code
      pure $ ExitFailure code
