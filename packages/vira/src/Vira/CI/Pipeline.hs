{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.FileSystem (FileSystem)
import Effectful.Git (Commit (..))
import Effectful.Git.Command.Clone qualified as Git
import Effectful.Git.Command.Status (GitStatusPorcelain (..), gitStatusPorcelain)
import Effectful.Reader.Static qualified as ER
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Environment qualified as Env
import Vira.CI.Error
import Vira.CI.Log (ViraLog (..), renderViraLogCLI)
import Vira.CI.Pipeline.Effect (PipelineEnv (..))
import Vira.CI.Pipeline.Handler qualified as Handler
import Vira.CI.Pipeline.Program (runPipelineProgram)
import Vira.CI.Pipeline.Type
import Vira.State.Type (Branch (..), Repo (..), cloneUrl)
import Vira.Supervisor.Process (runProcesses)
import Vira.Tool.Core (Tools, getAllTools)

-- | Run `ViraPipeline` for the given `ViraEnvironment`
runPipeline ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  ) =>
  ViraEnvironment ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es (Either PipelineError ExitCode)
runPipeline env logger = do
  let outputLog = Just $ env.workspacePath </> "output.log"
  -- 1. Setup workspace and clone
  let setupProcs =
        one $ Git.cloneAtCommit env.repo.cloneUrl env.branch.headCommit.id Env.projectDirName
  runProcesses env.workspacePath outputLog logger setupProcs >>= \case
    Left err ->
      pure $ Left $ PipelineTerminated err
    Right ExitSuccess -> do
      let repoDir = env.workspacePath </> Env.projectDirName
          ctx = Env.viraContext env
          tools = env.tools
      runPipelineIn tools ctx repoDir env outputLog logger
    Right exitCode -> do
      pure $ Right exitCode

-- | Like `runPipeline`, but in a specific directory with given context, tools, and environment.
runPipelineIn ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  ) =>
  Tools ->
  ViraContext ->
  FilePath ->
  ViraEnvironment ->
  Maybe FilePath ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es (Either PipelineError ExitCode)
runPipelineIn tools ctx repoDir viraEnv outputLog logger = do
  -- Create pipeline environment
  let pipelineEnv =
        PipelineEnv
          { workspaceDir = repoDir
          , outputLog = outputLog
          , tools = tools
          , viraContext = ctx
          , viraEnv = viraEnv
          }

  -- Run the pipeline program with the real handler
  runErrorNoCallStack $
    Handler.runPipeline pipelineEnv logger $
      runPipelineProgram repoDir

-- | CLI wrapper for running a pipeline in the current directory
runPipelineCLI ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  ) =>
  Severity ->
  FilePath ->
  Eff es (Either PipelineError ExitCode)
runPipelineCLI minSeverity repoDir = do
  -- Detect git branch and dirty status (fail if not in a git repo)
  porcelain <- runErrorNoCallStack (gitStatusPorcelain repoDir) >>= either error pure
  let ctx = ViraContext porcelain.branch porcelain.dirty
  tools <- getAllTools
  -- For CLI, create a stub ViraEnvironment (clone step won't be used)
  let stubEnv =
        ViraEnvironment
          { repo = error "CLI: repo not available"
          , branch = error "CLI: branch not available"
          , tools = tools
          , workspacePath = repoDir
          }
  runPipelineIn tools ctx repoDir stubEnv Nothing logger
  where
    logger :: forall es1. (IOE :> es1, ER.Reader LogContext :> es1) => Severity -> Text -> Eff es1 ()
    logger severity msg = do
      ctx <- ER.ask
      when (severity >= minSeverity) $
        liftIO $
          putTextLn $
            renderViraLogCLI (ViraLog {level = severity, message = msg, context = ctx})

-- | Create a default pipeline configuration
defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage (one defaultFlake)
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    }
  where
    defaultFlake = Flake "." mempty
