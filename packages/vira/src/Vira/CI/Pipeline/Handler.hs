{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Handler (
  runPipeline,
  runPipelineLocal,
  defaultPipeline,
) where

import Prelude hiding (asks)

import Attic qualified
import Attic.Config (lookupEndpointWithToken)
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified
import Colog (Severity (..))
import Colog.Message (RichMessage)
import DevourFlake (DevourFlakeArgs (..), devourFlake)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.Git.Command.Clone qualified as Git
import Effectful.Git.Types (Commit (..))
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import GH.Signoff qualified as Signoff
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Nix.Core (nix)
import System.Nix.System (nixSystem)
import System.Process (proc)
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Environment qualified as Env
import Vira.CI.Error (ConfigurationError (..), PipelineError (..))
import Vira.CI.Pipeline.Effect
import Vira.CI.Pipeline.Program (runPipelineProgramLocal)
import Vira.CI.Pipeline.Type (BuildStage (..), CacheStage (..), Flake (..), SignoffStage (..), ViraPipeline (..))
import Vira.State.Type (Branch (..), Repo (..))
import Vira.Supervisor.Process (runProcesses)
import Vira.Tool.Core (ToolError (..))
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Type.ToolData (status)
import Vira.Tool.Type.Tools (attic)

-- | Default pipeline configuration
defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage (one defaultFlake)
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    }
  where
    defaultFlake = Flake "." mempty

-- | Run the PipelineLocal effect (core operations, no clone)
runPipelineLocal ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  PipelineLocalEnv ->
  FilePath ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff (PipelineLocal : es) a ->
  Eff es a
runPipelineLocal env workDir logger = interpret $ \_ -> \case
  LoadConfig -> loadConfigImpl env workDir logger
  Build pipeline -> buildImpl env workDir pipeline logger
  Cache pipeline buildResults -> cacheImpl env workDir pipeline buildResults logger
  Signoff pipeline -> signoffImpl env workDir pipeline logger
  LogPipeline severity msg -> logger severity msg

{- | Run the full pipeline (clone + local operations)
Not a generic effect handler - directly implements the full pipeline flow
-}
runPipeline ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  PipelineEnv ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es ()
runPipeline env logger = do
  logger Info "Starting pipeline with clone"

  -- Execute clone to get the directory
  cloneResults <- cloneImpl env logger
  logger Info $ "Repository cloned to " <> toText cloneResults.repoDir

  -- Now run the local pipeline in the cloned directory
  let clonedDir = env.viraEnv.workspacePath </> cloneResults.repoDir
  runPipelineLocal env.localEnv clonedDir logger runPipelineProgramLocal

-- | Implementation: Clone repository
cloneImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  PipelineEnv ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es CloneResults
cloneImpl env logger = do
  -- Clone effect decides the directory name locally
  let repoDir = "project"
      cloneProc =
        Git.cloneAtCommit
          env.viraEnv.repo.cloneUrl
          env.viraEnv.branch.headCommit.id
          repoDir

  logger Info $ "Cloning repository at commit " <> toText env.viraEnv.branch.headCommit.id

  result <- runProcesses env.viraEnv.workspacePath env.localEnv.outputLog logger (one cloneProc)

  case result of
    Left err -> throwError $ PipelineTerminated err
    Right ExitSuccess -> do
      logger Info $ "Repository cloned to " <> toText repoDir
      pure $
        CloneResults
          { repoDir = repoDir
          , commitId = env.viraEnv.branch.headCommit.id
          }
    Right exitCode@(ExitFailure code) -> do
      logger Error $ "Clone failed with exit code " <> show code
      throwError $ PipelineProcessFailed exitCode

-- | Implementation: Load vira.hs configuration
loadConfigImpl ::
  ( FileSystem :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  PipelineLocalEnv ->
  FilePath ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es ViraPipeline
loadConfigImpl env workDir logger = do
  let viraConfigPath = workDir </> "vira.hs"
  doesFileExist viraConfigPath >>= \case
    True -> do
      logger Info "Found vira.hs configuration file, applying customizations..."
      content <- liftIO $ decodeUtf8 <$> readFileBS viraConfigPath
      Configuration.applyConfig content env.viraContext defaultPipeline >>= \case
        Left err -> throwError $ PipelineConfigurationError $ InterpreterError err
        Right p -> do
          logger Info "Successfully applied vira.hs configuration"
          pure $ patchPipelineForDirty env.viraContext p
    False -> do
      logger Info "No vira.hs found - using default pipeline"
      pure $ patchPipelineForDirty env.viraContext defaultPipeline
  where
    -- Certain stages don't make sense when running CI on a dirty working copy
    patchPipelineForDirty :: ViraContext -> ViraPipeline -> ViraPipeline
    patchPipelineForDirty ctx pipeline
      | ctx.dirty =
          pipeline
            { -- Can't signoff on commit when build was on dirty working copy
              signoff = pipeline.signoff {enable = False}
            , -- Don't push unless on clean branch
              cache = pipeline.cache {url = Nothing}
            }
      | otherwise = pipeline

-- | Implementation: Build flakes
buildImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  PipelineLocalEnv ->
  FilePath ->
  ViraPipeline ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es BuildResults
buildImpl env workDir pipeline logger = do
  logger Info $ "Building " <> show (length pipeline.build.flakes) <> " flakes"

  -- Build each flake sequentially (to match current behavior)
  results <- forM (toList pipeline.build.flakes) $ \flake -> do
    buildFlake env workDir flake logger

  case nonEmpty results of
    Nothing -> throwError $ PipelineConfigurationError $ MalformedConfig "No flakes to build"
    Just ne -> pure $ BuildResults {results = ne}

-- | Build a single flake
buildFlake ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  PipelineLocalEnv ->
  FilePath ->
  Flake ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es FilePath
buildFlake env workDir (Flake flakePath overrideInputs) logger = do
  let buildProc = proc nix $ devourFlake args
      args =
        DevourFlakeArgs
          { flakePath = flakePath
          , systems = Nothing
          , outLink = Just (flakePath </> "result")
          , overrideInputs = overrideInputs
          }

  logger Info $ "Building flake at " <> toText flakePath

  -- Run build process from working directory
  result <- runProcesses workDir env.outputLog logger (one buildProc)

  case result of
    Left err -> throwError $ PipelineTerminated err
    Right ExitSuccess -> do
      -- Return relative path to result symlink (relative to repo root)
      let resultPath = flakePath </> "result"
      logger Info $ "Build succeeded, result at " <> toText resultPath
      pure resultPath
    Right exitCode@(ExitFailure code) -> do
      logger Error $ "Build failed for " <> toText flakePath <> " with exit code " <> show code
      throwError $ PipelineProcessFailed exitCode

-- | Implementation: Push to cache
cacheImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  PipelineLocalEnv ->
  FilePath ->
  ViraPipeline ->
  BuildResults ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es ()
cacheImpl env workDir pipeline buildResults logger = do
  case pipeline.cache.url of
    Nothing -> do
      logger Info "Cache disabled, skipping"
    Just urlText -> do
      logger Info $ "Pushing " <> show (length buildResults.results) <> " results to cache"

      -- Parse cache URL
      (serverEndpoint, cacheName) <- case Attic.Url.parseCacheUrl urlText of
        Left err -> throwError $ parseErrorToPipelineError urlText err
        Right result -> pure result

      -- Get attic server info (token validated by lookupEndpointWithToken)
      server <- case do
        atticConfig <- env.tools.attic.status
        -- Get server name for endpoint (only if it has a token)
        serverName <-
          lookupEndpointWithToken atticConfig serverEndpoint
            & maybeToRight (AtticTool.MissingEndpoint serverEndpoint)
        -- Create server (token already validated by lookupEndpointWithToken)
        pure $ AtticServer serverName serverEndpoint of
        Left err -> throwError $ atticErrorToPipelineError urlText serverEndpoint err
        Right result -> pure result

      -- Push all results to cache - paths are relative to workDir
      logger Info $ "Pushing " <> show (length buildResults.results) <> " paths: " <> show (toList buildResults.results)
      let pushProc = Attic.atticPushProcess server cacheName buildResults.results
      runProcesses workDir env.outputLog logger (one pushProc) >>= \case
        Left err -> throwError $ PipelineTerminated err
        Right ExitSuccess -> logger Info "Cache push succeeded"
        Right exitCode@(ExitFailure code) -> do
          logger Error $ "Cache push failed with exit code " <> show code
          throwError $ PipelineProcessFailed exitCode
  where
    parseErrorToPipelineError :: Text -> Attic.Url.ParseError -> PipelineError
    parseErrorToPipelineError url err =
      PipelineConfigurationError $
        MalformedConfig $
          "Invalid cache URL '" <> url <> "': " <> show err

    atticErrorToPipelineError :: Text -> AtticServerEndpoint -> AtticTool.ConfigError -> PipelineError
    atticErrorToPipelineError url _endpoint err =
      let suggestion = AtticTool.configErrorToSuggestion err
          suggestionText =
            "\n\nSuggestion: Run the following in your terminal\n\n"
              <> show @Text suggestion
          msg = "Attic configuration error for cache URL '" <> url <> "': " <> show err <> suggestionText
       in PipelineToolError $ ToolError msg

-- | Implementation: Create signoff
signoffImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  PipelineLocalEnv ->
  FilePath ->
  ViraPipeline ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es ()
signoffImpl env workDir pipeline logger = do
  if pipeline.signoff.enable
    then do
      logger Info "Creating commit signoff"
      let signoffProc = Signoff.create Signoff.Force statusTitle
          statusTitle = "vira/" <> toString nixSystem <> "/ci"
      runProcesses workDir env.outputLog logger (one signoffProc) >>= \case
        Left err -> throwError $ PipelineTerminated err
        Right ExitSuccess -> logger Info "Signoff succeeded"
        Right exitCode@(ExitFailure code) -> do
          logger Error $ "Signoff failed with exit code " <> show code
          throwError $ PipelineProcessFailed exitCode
    else
      logger Info "Signoff disabled, skipping"
