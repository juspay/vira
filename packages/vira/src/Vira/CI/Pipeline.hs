{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline (runPipeline, defaultPipeline, PipelineError (..)) where

import Prelude hiding (id)

import Colog (Message)
import Effectful.Colog (Log)
import Effectful.Process (Process)

import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.Git (Commit (..))
import Effectful.Git qualified as Git
import Language.Haskell.Interpreter (InterpreterError (..))
import Shower qualified
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Environment qualified as Env
import Vira.CI.Error
import Vira.CI.Pipeline.Type
import Vira.CI.Processes (pipelineProcesses)
import Vira.State.Type (Branch (..), cloneUrl)
import Vira.Supervisor.Process (runProcesses)
import Vira.Supervisor.Type (TaskId)

-- | Run `ViraPipeline` for the given `ViraEnvironment`
runPipeline ::
  ( Concurrent :> es
  , Process :> es
  , Log Message :> es
  , IOE :> es
  , FileSystem :> es
  ) =>
  ViraEnvironment ->
  TaskId ->
  (forall es1. (IOE :> es1) => Text -> Eff es1 ()) ->
  Eff es (Either PipelineError ExitCode)
runPipeline env taskId logger = do
  -- 1. Setup workspace and clone
  let setupProcs =
        one $ Git.cloneAtCommit env.repo.cloneUrl env.branch.headCommit.id Env.projectDirName
  runProcesses taskId env.workspacePath logger setupProcs >>= \case
    Left err ->
      pure $ Left $ PipelineTerminated err
    Right ExitSuccess -> do
      -- 2. Configure the pipeline, looking for optional vira.hs
      runErrorNoCallStack @InterpreterError (environmentPipeline env logger) >>= \case
        Left err -> do
          pure $ Left $ PipelineConfigurationError $ InterpreterError err
        Right pipeline -> do
          logger $ toText $ "ℹ️ Pipeline configuration:\n" <> Shower.shower pipeline
          case pipelineProcesses env pipeline of
            Left err -> do
              pure $ Left err
            Right pipelineProcs -> do
              -- 3. Run the actual CI pipeline.
              runProcesses taskId env.workspacePath logger pipelineProcs <&> first PipelineTerminated
    Right exitCode -> do
      pure $ Right exitCode

{- | Load pipeline configuration for the given project environment.

Uses vira.hs from project repository.
-}
environmentPipeline ::
  (Error InterpreterError :> es, IOE :> es) =>
  -- | Project's environment
  ViraEnvironment ->
  -- | Logger function
  (forall es1. (IOE :> es1) => Text -> Eff es1 ()) ->
  Eff es ViraPipeline
environmentPipeline env logger = do
  let
    pipeline = defaultPipeline
    viraConfigPath = Env.projectDir env </> "vira.hs"
  configExists <- liftIO $ doesFileExist viraConfigPath

  if configExists
    then do
      logger "Found vira.hs configuration file, applying customizations..."
      content <- liftIO $ readFileBS viraConfigPath
      liftIO (Configuration.applyConfig (decodeUtf8 content) (Env.viraContext env) pipeline) >>= \case
        Left err -> do
          throwError err
        Right customPipeline -> do
          logger "Successfully applied vira.hs configuration"
          pure customPipeline
    else do
      logger "No vira.hs found - using default pipeline"
      pure pipeline

-- | Create a default pipeline configuration
defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage mempty
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    }
