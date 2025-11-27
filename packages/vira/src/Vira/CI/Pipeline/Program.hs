{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Program where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static qualified as ER
import Shower qualified
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Error (PipelineError (..))
import Vira.CI.Pipeline.Effect
import Vira.State.Type (Branch, Repo)

-- | Pipeline program for CLI (uses existing local directory)
pipelineProgram ::
  ( Pipeline :> es
  , ER.Reader PipelineEnv :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , IOE :> es
  , Error PipelineError :> es
  ) =>
  Eff es ()
pipelineProgram = do
  logPipeline Info "Starting pipeline execution"

  -- Step 1: Load configuration
  pipeline <- loadConfig
  logPipeline Info $ toText $ "Pipeline configuration:\n" <> Shower.shower pipeline

  -- Step 2: Build
  buildResults <- build pipeline
  logPipeline Info $ "Built " <> show (length buildResults) <> " flakes"

  -- Step 3: Cache using build results
  cache pipeline buildResults

  -- Step 4: Signoff
  signoff pipeline buildResults
  logPipeline Info "Pipeline completed successfully"

{- | Pipeline program with clone (for web/CI)
Clones repository first, then runs pipeline
-}
pipelineProgramWithClone ::
  ( Pipeline :> es
  , ER.Reader PipelineEnv :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , IOE :> es
  , Error PipelineError :> es
  ) =>
  Repo ->
  Branch ->
  FilePath ->
  Eff es ()
pipelineProgramWithClone repo branch workspacePath = do
  -- Step 1: Clone repository
  clonedDir <- clone repo branch workspacePath

  -- Step 2-5: Run pipeline in the cloned directory
  -- HACK: Update context with actual cloned directory
  ER.local @PipelineEnv
    ( \env ->
        let oldCtx = env.viraContext
            newCtx = ViraContext oldCtx.branch oldCtx.onlyBuild oldCtx.commitId oldCtx.cloneUrl clonedDir
         in PipelineEnv env.outputLog env.tools newCtx env.logger
    )
    pipelineProgram
