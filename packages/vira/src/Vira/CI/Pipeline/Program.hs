{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Program where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static qualified as ER
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import System.Nix.System (System (..))
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Error (PipelineError (..))
import Vira.CI.Pipeline.Effect
import Vira.CI.Pipeline.Type (BuildStage (..), CacheStage (..), Flake (..), SignoffStage (..), ViraPipeline (..))
import Vira.State.Type (Branch, Repo)

-- | Pretty-print pipeline configuration in a concise format
prettyPipeline :: ViraPipeline -> Text
prettyPipeline ViraPipeline {build = buildStage, cache = cacheStage, signoff = signoffStage} =
  renderStrict $
    layoutPretty defaultLayoutOptions $
      vsep
        [ "Build:"
        , indent 2 $
            vsep
              [ "Flakes:" <+> pretty (NE.length buildStage.flakes) <+> "flake(s)"
              , vsep (map prettyFlake (NE.toList buildStage.flakes))
              , "Systems:" <+> hsep (punctuate comma (map (\(System s) -> pretty s) buildStage.systems))
              ]
        , "Cache:" <+> maybe "disabled" (\url -> "enabled" <+> parens (pretty url)) cacheStage.url
        , "Signoff:" <+> if signoffStage.enable then "enabled" else "disabled"
        ]
  where
    prettyFlake :: Flake -> Doc ann
    prettyFlake f =
      "-" <+> pretty (toText f.path) <> prettyOverrides f.overrideInputs

    prettyOverrides :: [(Text, Text)] -> Doc ann
    prettyOverrides [] = mempty
    prettyOverrides ovs =
      space <> parens ("overrides:" <+> hsep (punctuate comma (map (\(k, v) -> pretty k <> "=" <> pretty v) ovs)))

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
  logPipeline Info $ "Pipeline configuration:\n" <> prettyPipeline pipeline

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
        let newCtx = env.viraContext {repoDir = clonedDir}
         in env {viraContext = newCtx}
    )
    pipelineProgram
