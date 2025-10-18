{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline (runPipeline, runPipelineCLI, defaultPipeline, PipelineError (..)) where

import Prelude hiding (id)

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Effectful.Colog (Log)
import Effectful.Process (Process)

import Effectful (Eff, IOE, (:>))
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem)
import Effectful.Git.Command.Status (GitStatusPorcelain (..), gitStatusPorcelain)
import Effectful.Reader.Static qualified as ER
import System.FilePath ((</>))
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Environment qualified as Env
import Vira.CI.Error
import Vira.CI.Log (ViraLog (..), renderViraLogCLI)
import Vira.CI.Pipeline.Effect (PipelineEnv (..), PipelineLocalEnv (..))
import Vira.CI.Pipeline.Handler (defaultPipeline)
import Vira.CI.Pipeline.Handler qualified as Handler
import Vira.CI.Pipeline.Program (runPipelineProgram, runPipelineProgramLocal)
import Vira.Tool.Core (getAllTools)

-- | Run `ViraPipeline` for the given `ViraEnvironment`
runPipeline ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  ViraEnvironment ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es ()
runPipeline env logger = do
  let outputLog = Just $ env.workspacePath </> "output.log"
      ctx = Env.viraContext env
      tools = env.tools
      localEnv =
        PipelineLocalEnv
          { baseDir = env.workspacePath
          , outputLog = outputLog
          , tools = tools
          , viraContext = ctx
          }
      pipelineEnv =
        PipelineEnv
          { localEnv = localEnv
          , viraEnv = env
          }

  -- Run the pipeline program with the real handler (includes Clone effect)
  Handler.runPipeline
    pipelineEnv
    logger
    runPipelineProgram

-- | CLI wrapper for running a pipeline in the current directory
runPipelineCLI ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  Severity ->
  FilePath ->
  Eff es ()
runPipelineCLI minSeverity repoDir = do
  -- Detect git branch and dirty status (fail if not in a git repo)
  porcelain <- runErrorNoCallStack (gitStatusPorcelain repoDir) >>= either error pure
  let ctx = ViraContext porcelain.branch porcelain.dirty
  tools <- getAllTools

  -- For CLI, use PipelineLocalEnv (no ViraEnvironment needed)
  let localEnv =
        PipelineLocalEnv
          { baseDir = repoDir
          , outputLog = Nothing
          , tools = tools
          , viraContext = ctx
          }

  -- Run local pipeline program directly
  Handler.runPipelineLocal localEnv logger $
    runPipelineProgramLocal repoDir
  where
    logger :: forall es1. (IOE :> es1, ER.Reader LogContext :> es1) => Severity -> Text -> Eff es1 ()
    logger severity msg = do
      ctx <- ER.ask
      when (severity >= minSeverity) $
        liftIO $
          putTextLn $
            renderViraLogCLI (ViraLog {level = severity, message = msg, context = ctx})
