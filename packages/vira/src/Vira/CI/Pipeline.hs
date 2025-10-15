{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline (runPipeline, defaultPipeline, PipelineError (..)) where

import Prelude hiding (id)

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Effectful.Colog (Log)
import Effectful.Process (Process)

import Effectful (Eff, IOE, (:>))
import Effectful.Colog.Simple (LogContext)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.Git (Commit (..))
import Effectful.Git qualified as Git
import Effectful.Reader.Static qualified as ER
import Language.Haskell.Interpreter (InterpreterError (..))
import Shower qualified
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (ViraContext)
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Environment qualified as Env
import Vira.CI.Error
import Vira.CI.Pipeline.Type
import Vira.CI.Processes (pipelineProcesses)
import Vira.State.Type (Branch (..), Repo (..), cloneUrl)
import Vira.Supervisor.Process (runProcesses)
import Vira.Tool.Core (Tools)

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
      runPipelineIn tools ctx repoDir outputLog logger
    Right exitCode -> do
      pure $ Right exitCode

-- | Like `runPipeline`, but in a specific directory with given context and tools.
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
  Maybe FilePath ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es (Either PipelineError ExitCode)
runPipelineIn tools ctx repoDir outputLog logger = do
  -- 2. Configure the pipeline, looking for optional vira.hs
  let viraConfigPath = repoDir </> "vira.hs"
  runErrorNoCallStack (loadViraHsConfiguration viraConfigPath ctx logger) >>= \case
    Left err -> do
      pure $ Left $ PipelineConfigurationError $ InterpreterError err
    Right pipeline -> do
      logger Info $ toText $ "Pipeline configuration:\n" <> Shower.shower pipeline
      case pipelineProcesses tools pipeline of
        Left err -> do
          pure $ Left err
        Right pipelineProcs -> do
          -- 3. Run the actual CI pipeline.
          runProcesses repoDir outputLog logger pipelineProcs <&> first PipelineTerminated

{- | Load vira.hs configuration if it exists.

Returns Nothing if vira.hs is not found, or Just pipeline if found and successfully loaded.
-}
loadViraHsConfiguration ::
  ( Error InterpreterError :> es
  , FileSystem :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  ) =>
  -- | Path to vira.hs configuration file
  FilePath ->
  -- | Vira context
  ViraContext ->
  -- | Logger function
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es ViraPipeline
loadViraHsConfiguration path ctx logger = do
  doesFileExist path >>= \case
    True -> do
      logger Info "Found vira.hs configuration file, applying customizations..."
      content <- liftIO $ decodeUtf8 <$> readFileBS path
      Configuration.applyConfig content ctx defaultPipeline >>= \case
        Left err -> do
          throwError err
        Right p -> do
          logger Info "Successfully applied vira.hs configuration"
          pure p
    False -> do
      logger Info "No vira.hs found - using default pipeline"
      pure defaultPipeline

-- | Create a default pipeline configuration
defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage mempty
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    }
