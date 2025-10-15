{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline (runPipeline, defaultPipeline, PipelineError (..)) where

import Prelude hiding (id)

import Colog.Message (RichMessage)
import Effectful.Colog (Log)
import Effectful.Process (Process)

import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.Git (Commit (..))
import Effectful.Git qualified as Git
import Effectful.Reader.Static qualified as ER
import Language.Haskell.Interpreter (InterpreterError (..))
import Shower qualified
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (ViraContext)
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Environment qualified as Env
import Vira.CI.Error
import Vira.CI.Pipeline.Type
import Vira.CI.Processes (pipelineProcesses)
import Vira.Lib.Logging (LogContext)
import Vira.State.Type (Branch (..), Repo (..), cloneUrl)
import Vira.Supervisor.Process (runProcesses)

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
  (forall es1. (IOE :> es1) => Text -> Eff es1 ()) ->
  Eff es (Either PipelineError ExitCode)
runPipeline env logger = do
  let outputLog = Just $ env.workspacePath </> "output.log"
  -- 1. Setup workspace and clone
  let setupProcs =
        one $ Git.cloneAtCommit env.repo.cloneUrl env.branch.headCommit.id Env.projectDirName
  runProcesses env.workspacePath outputLog logger setupProcs >>= \case
    Left err ->
      pure $ Left $ PipelineTerminated err
    Right ExitSuccess ->
      configureAndRunPipeline env outputLog logger
    Right exitCode -> do
      pure $ Right exitCode

-- | Configure and execute the pipeline
configureAndRunPipeline ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  ) =>
  ViraEnvironment ->
  Maybe FilePath ->
  (forall es1. (IOE :> es1) => Text -> Eff es1 ()) ->
  Eff es (Either PipelineError ExitCode)
configureAndRunPipeline env outputLog logger = do
  -- 2. Configure the pipeline, looking for optional vira.hs
  let viraConfigPath = Env.projectDir env </> "vira.hs"
  runErrorNoCallStack (loadViraHsConfiguration viraConfigPath (Env.viraContext env) logger) >>= \case
    Left err -> do
      pure $ Left $ PipelineConfigurationError $ InterpreterError err
    Right pipeline -> do
      logger $ toText $ "ℹ️ Pipeline configuration:\n" <> Shower.shower pipeline
      case pipelineProcesses env.tools pipeline of
        Left err -> do
          pure $ Left err
        Right pipelineProcs -> do
          -- 3. Run the actual CI pipeline.
          runProcesses env.workspacePath outputLog logger pipelineProcs <&> first PipelineTerminated

{- | Load vira.hs configuration if it exists.

Returns Nothing if vira.hs is not found, or Just pipeline if found and successfully loaded.
-}
loadViraHsConfiguration ::
  (Error InterpreterError :> es, IOE :> es) =>
  -- | Path to vira.hs configuration file
  FilePath ->
  -- | Vira context
  ViraContext ->
  -- | Logger function
  (forall es1. (IOE :> es1) => Text -> Eff es1 ()) ->
  Eff es ViraPipeline
loadViraHsConfiguration viraConfigPath ctx logger = do
  configExists <- liftIO $ doesFileExist viraConfigPath

  if configExists
    then do
      logger "Found vira.hs configuration file, applying customizations..."
      content <- liftIO $ readFileBS viraConfigPath
      liftIO (Configuration.applyConfig (decodeUtf8 content) ctx defaultPipeline) >>= \case
        Left err -> do
          throwError err
        Right customPipeline -> do
          logger "Successfully applied vira.hs configuration"
          pure customPipeline
    else do
      logger "No vira.hs found - using default pipeline"
      pure defaultPipeline

-- | Create a default pipeline configuration
defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage mempty
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    }
