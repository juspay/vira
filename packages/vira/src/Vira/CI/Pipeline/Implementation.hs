{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Implementation (
  runRemotePipeline,
  runCLIPipeline,
  defaultPipeline,
) where

import Prelude hiding (asks, id)

import Attic qualified
import Attic.Config (lookupEndpointWithToken)
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified
import Colog (Severity (..))
import Colog.Message (RichMessage)
import DevourFlake (DevourFlakeArgs (..), devourFlake)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.Git.Command.Clone qualified as Git
import Effectful.Git.Command.Status (GitStatusPorcelain (..), gitStatusPorcelain)
import Effectful.Git.Types (Commit (..))
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import GH.Signoff qualified as Signoff
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Nix.Core (nix)
import System.Nix.System (nixSystem)
import System.Process (CreateProcess, proc)
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Environment qualified as Env
import Vira.CI.Error (ConfigurationError (..), PipelineError (..))
import Vira.CI.Log (ViraLog (..), renderViraLogCLI)
import Vira.CI.Pipeline.Effect
import Vira.CI.Pipeline.Type (BuildStage (..), CacheStage (..), Flake (..), SignoffStage (..), ViraPipeline (..))
import Vira.State.Type (Branch (..), Repo (..))
import Vira.Supervisor.Process (runProcesses)
import Vira.Supervisor.Type (Terminated (..))
import Vira.Tool.Core (ToolError (..), getAllTools)
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Type.ToolData (status)
import Vira.Tool.Type.Tools (Tools, attic)

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

-- | Run remote pipeline for the given `ViraEnvironment`
runRemotePipeline ::
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
  (forall es1. (PipelineRemote :> es1, ER.Reader PipelineLocalEnv :> es1, Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1, Error PipelineError :> es1) => Eff es1 ()) ->
  Eff es ()
runRemotePipeline env logger program = do
  -- Run the remote pipeline program with the handler (includes Clone effect)
  runPipelineRemote (pipelineRemoteEnvFrom env logger) program

pipelineRemoteEnvFrom :: ViraEnvironment -> (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) -> PipelineRemoteEnv
pipelineRemoteEnvFrom viraEnv logger =
  PipelineRemoteEnv
    { localEnv = localEnvFromViraEnv viraEnv
    , viraEnv = viraEnv
    }
  where
    localEnvFromViraEnv :: ViraEnvironment -> PipelineLocalEnv
    localEnvFromViraEnv env' =
      let outputLog = Just $ env'.workspacePath </> "output.log"
          tools = env'.tools
          ctx = Env.viraContext env'
       in PipelineLocalEnv {outputLog = outputLog, tools = tools, viraContext = ctx, logger = PipelineLogger logger}

-- | CLI wrapper for running a pipeline in the current directory
runCLIPipeline ::
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
  (forall es1. (PipelineLocal :> es1, ER.Reader PipelineLocalEnv :> es1, Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1, Error PipelineError :> es1) => Eff es1 ()) ->
  Eff es ()
runCLIPipeline minSeverity repoDir program = do
  -- Detect git branch and dirty status (fail if not in a git repo)
  porcelain <- runErrorNoCallStack (gitStatusPorcelain repoDir) >>= either error pure
  let ctx = ViraContext porcelain.branch porcelain.dirty
  tools <- getAllTools

  -- Run local pipeline program directly (workDir = repoDir for CLI)
  runPipelineLocal (pipelineLocalEnvFrom minSeverity tools ctx) repoDir program

pipelineLocalEnvFrom :: Severity -> Tools -> ViraContext -> PipelineLocalEnv
pipelineLocalEnvFrom minSeverity tools ctx =
  PipelineLocalEnv
    { outputLog = Nothing
    , tools = tools
    , viraContext = ctx
    , logger = PipelineLogger logger
    }
  where
    logger :: forall es1. (IOE :> es1, ER.Reader LogContext :> es1) => Severity -> Text -> Eff es1 ()
    logger severity msg = do
      logCtx <- ER.ask
      when (severity >= minSeverity) $
        liftIO $
          putTextLn $
            renderViraLogCLI (ViraLog {level = severity, message = msg, context = logCtx})

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
  Eff (PipelineLocal : ER.Reader PipelineLocalEnv : es) a ->
  Eff es a
runPipelineLocal env workDir program =
  ER.runReader env $
    interpret
      ( \_ -> \case
          LoadConfig -> loadConfigImpl workDir
          Build pipeline -> buildImpl workDir pipeline
          Cache pipeline buildResults -> cacheImpl workDir pipeline buildResults
          Signoff pipeline -> signoffImpl workDir pipeline
      )
      program

-- | Run the PipelineRemote effect (handles Clone + RunLocalPipeline)
runPipelineRemote ::
  forall es a.
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  PipelineRemoteEnv ->
  Eff (PipelineRemote : ER.Reader PipelineLocalEnv : es) a ->
  Eff es a
runPipelineRemote env program =
  ER.runReader env.localEnv $
    interpret
      ( \_ -> \case
          Clone -> ER.runReader env cloneImpl
          RunLocalPipeline cloneResults localProgram ->
            let clonedDir = env.viraEnv.workspacePath </> cloneResults.repoDir
             in raise $ runPipelineLocal env.localEnv clonedDir localProgram
      )
      program

-- | Helper: Run processes with logger from effect
runProcesses' ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineLocalEnv :> es
  ) =>
  FilePath ->
  Maybe FilePath ->
  NonEmpty CreateProcess ->
  Eff es (Either Terminated ExitCode)
runProcesses' workDir outputLog procs = do
  env <- ER.ask @PipelineLocalEnv
  runProcesses workDir outputLog (unPipelineLogger env.logger) procs

-- | Implementation: Clone repository
cloneImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineRemoteEnv :> es
  , ER.Reader PipelineLocalEnv :> es
  , Error PipelineError :> es
  ) =>
  Eff es CloneResults
cloneImpl = do
  env <- ER.ask @PipelineRemoteEnv
  let cloneProc =
        Git.cloneAtCommit
          env.viraEnv.repo.cloneUrl
          env.viraEnv.branch.headCommit.id
          Env.projectDirName

  logPipeline Info $ "Cloning repository at commit " <> toText env.viraEnv.branch.headCommit.id

  result <- runProcesses' env.viraEnv.workspacePath env.localEnv.outputLog (one cloneProc)

  case result of
    Left err -> throwError $ PipelineTerminated err
    Right ExitSuccess -> do
      logPipeline Info $ "Repository cloned to " <> toText Env.projectDirName
      pure $
        CloneResults
          { repoDir = Env.projectDirName
          , commitId = env.viraEnv.branch.headCommit.id
          }
    Right exitCode@(ExitFailure code) -> do
      logPipeline Error $ "Clone failed with exit code " <> show code
      throwError $ PipelineProcessFailed exitCode

-- | Implementation: Load vira.hs configuration
loadConfigImpl ::
  ( FileSystem :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineLocalEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  Eff es ViraPipeline
loadConfigImpl workDir = do
  env <- ER.ask @PipelineLocalEnv
  let viraConfigPath = workDir </> "vira.hs"
  doesFileExist viraConfigPath >>= \case
    True -> do
      logPipeline Info "Found vira.hs configuration file, applying customizations..."
      content <- liftIO $ decodeUtf8 <$> readFileBS viraConfigPath
      Configuration.applyConfig content env.viraContext defaultPipeline >>= \case
        Left err -> throwError $ PipelineConfigurationError $ InterpreterError err
        Right p -> do
          logPipeline Info "Successfully applied vira.hs configuration"
          pure $ patchPipelineForDirty env.viraContext p
    False -> do
      logPipeline Info "No vira.hs found - using default pipeline"
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
  , ER.Reader PipelineLocalEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  ViraPipeline ->
  Eff es BuildResults
buildImpl workDir pipeline = do
  logPipeline Info $ "Building " <> show (length pipeline.build.flakes) <> " flakes"

  -- Build each flake sequentially (to match current behavior)
  results <- forM (toList pipeline.build.flakes) $ \flake -> do
    buildFlake workDir flake

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
  , ER.Reader PipelineLocalEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  Flake ->
  Eff es FilePath
buildFlake workDir (Flake flakePath overrideInputs) = do
  env <- ER.ask @PipelineLocalEnv
  let buildProc = proc nix $ devourFlake args
      args =
        DevourFlakeArgs
          { flakePath = flakePath
          , systems = Nothing
          , outLink = Just (flakePath </> "result")
          , overrideInputs = overrideInputs
          }

  logPipeline Info $ "Building flake at " <> toText flakePath

  -- Run build process from working directory
  result <- runProcesses' workDir env.outputLog (one buildProc)

  case result of
    Left err -> throwError $ PipelineTerminated err
    Right ExitSuccess -> do
      -- Return relative path to result symlink (relative to repo root)
      let resultPath = flakePath </> "result"
      logPipeline Info $ "Build succeeded, result at " <> toText resultPath
      pure resultPath
    Right exitCode@(ExitFailure code) -> do
      logPipeline Error $ "Build failed for " <> toText flakePath <> " with exit code " <> show code
      throwError $ PipelineProcessFailed exitCode

-- | Implementation: Push to cache
cacheImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineLocalEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  ViraPipeline ->
  BuildResults ->
  Eff es ()
cacheImpl workDir pipeline buildResults = do
  env <- ER.ask @PipelineLocalEnv
  case pipeline.cache.url of
    Nothing -> do
      logPipeline Info "Cache disabled, skipping"
    Just urlText -> do
      logPipeline Info $ "Pushing " <> show (length buildResults.results) <> " results to cache"

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
      logPipeline Info $ "Pushing " <> show (length buildResults.results) <> " paths: " <> show (toList buildResults.results)
      let pushProc = Attic.atticPushProcess server cacheName buildResults.results
      runProcesses' workDir env.outputLog (one pushProc) >>= \case
        Left err -> throwError $ PipelineTerminated err
        Right ExitSuccess -> logPipeline Info "Cache push succeeded"
        Right exitCode@(ExitFailure code) -> do
          logPipeline Error $ "Cache push failed with exit code " <> show code
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
  , ER.Reader PipelineLocalEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  ViraPipeline ->
  Eff es ()
signoffImpl workDir pipeline = do
  env <- ER.ask @PipelineLocalEnv
  if pipeline.signoff.enable
    then do
      logPipeline Info "Creating commit signoff"
      let signoffProc = Signoff.create Signoff.Force statusTitle
          statusTitle = "vira/" <> toString nixSystem <> "/ci"
      runProcesses' workDir env.outputLog (one signoffProc) >>= \case
        Left err -> throwError $ PipelineTerminated err
        Right ExitSuccess -> logPipeline Info "Signoff succeeded"
        Right exitCode@(ExitFailure code) -> do
          logPipeline Error $ "Signoff failed with exit code " <> show code
          throwError $ PipelineProcessFailed exitCode
    else
      logPipeline Info "Signoff disabled, skipping"
