{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Handler where

import Prelude hiding (asks)

import Attic qualified
import Attic.Config (AtticConfig (..), AtticServerConfig (..), lookupEndpointWithToken)
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
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
import System.Nix.System (nixSystem)
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Environment qualified as Env
import Vira.CI.Error (ConfigurationError (..), PipelineError (..))
import Vira.CI.Pipeline.Effect
import Vira.CI.Pipeline.ProcessHelpers (createBuildProcess)
import Vira.CI.Pipeline.Type (BuildStage (..), CacheStage (..), Flake (..), SignoffStage (..), ViraPipeline (..))
import Vira.State.Type (Branch (..), Repo (..))
import Vira.Supervisor.Process (runProcesses)
import Vira.Supervisor.Type (Terminated (..))
import Vira.Tool.Core (ToolError (..))
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Type.ToolData (status)
import Vira.Tool.Type.Tools (attic)

-- | Run the Pipeline effect with concrete implementations
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
  Eff (Pipeline : es) a ->
  Eff es a
runPipeline env logger = interpret $ \_ -> \case
  Clone -> cloneImpl env logger
  LoadConfig repoDir -> loadConfigImpl env repoDir logger
  Build repoDir pipeline -> buildImpl env repoDir pipeline logger
  Cache pipeline buildResults -> cacheImpl env pipeline buildResults logger
  Signoff pipeline -> signoffImpl env pipeline logger
  LogPipeline severity msg -> logger severity msg

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
  let cloneProc =
        Git.cloneAtCommit
          env.viraEnv.repo.cloneUrl
          env.viraEnv.branch.headCommit.id
          Env.projectDirName

  logger Info $ "Cloning repository at commit " <> toText env.viraEnv.branch.headCommit.id

  result <- runProcesses env.workspaceDir env.outputLog logger (one cloneProc)

  case result of
    Left err -> throwError $ PipelineTerminated err
    Right ExitSuccess -> do
      let repoDir = env.workspaceDir </> Env.projectDirName
      logger Info $ "Repository cloned to " <> toText repoDir
      pure $
        CloneResults
          { repoDir = repoDir
          , commitId = toText env.viraEnv.branch.headCommit.id
          }
    Right (ExitFailure _code) -> throwError $ PipelineTerminated Terminated

-- | Implementation: Load vira.hs configuration
loadConfigImpl ::
  ( FileSystem :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  ) =>
  PipelineEnv ->
  FilePath ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es ViraPipeline
loadConfigImpl env repoDir logger = do
  let viraConfigPath = repoDir </> "vira.hs"
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

    defaultPipeline :: ViraPipeline
    defaultPipeline =
      ViraPipeline
        { build = BuildStage (one defaultFlake)
        , cache = CacheStage Nothing
        , signoff = SignoffStage False
        }
      where
        defaultFlake = Flake "." mempty

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
  PipelineEnv ->
  FilePath ->
  ViraPipeline ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es BuildResults
buildImpl env repoDir pipeline logger = do
  logger Info $ "Building " <> show (length pipeline.build.flakes) <> " flakes"

  -- Build each flake sequentially (to match current behavior)
  results <- forM (toList pipeline.build.flakes) $ \flake -> do
    buildFlake env repoDir flake logger

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
  PipelineEnv ->
  FilePath ->
  Flake ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es BuildResult
buildFlake env repoDir flake logger = do
  let Flake flakePath _overrideInputs = flake
      flakeAbsPath = repoDir </> flakePath
      buildProc = createBuildProcess flake

  logger Info $ "Building flake at " <> toText flakeAbsPath

  -- Run build process (runProcesses sets working directory to repoDir)
  result <- runProcesses repoDir env.outputLog logger (one buildProc)

  case result of
    Left err -> throwError $ PipelineTerminated err
    Right ExitSuccess -> do
      -- For now, we'll use the result symlink path
      -- TODO: Capture stdout from devour-flake to get actual store paths
      let resultPath = flakeAbsPath </> "result"
          storePaths = [] -- TODO: Parse from stdout when available
      logger Info $ "Build succeeded, result at " <> toText resultPath

      pure $
        BuildResult
          { flakePath = flakePath
          , resultSymlink = resultPath
          , storePaths = storePaths
          }
    Right (ExitFailure _code) ->
      throwError $ PipelineTerminated Terminated

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
  PipelineEnv ->
  ViraPipeline ->
  BuildResults ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es ExitCode
cacheImpl env pipeline buildResults logger = do
  case pipeline.cache.url of
    Nothing -> do
      logger Info "Cache disabled, skipping"
      pure ExitSuccess
    Just urlText -> do
      let paths = resultPaths buildResults
      logger Info $ "Pushing " <> show (length paths) <> " results to cache"

      -- Parse cache URL
      (serverEndpoint, cacheName) <- case Attic.Url.parseCacheUrl urlText of
        Left err -> throwError $ parseErrorToPipelineError urlText err
        Right result -> pure result

      -- Get attic config and setup login
      (loginProc, server) <- case do
        atticConfig <- env.tools.attic.status
        -- Get server name for endpoint (only if it has a token)
        serverName <-
          lookupEndpointWithToken atticConfig serverEndpoint
            & maybeToRight (AtticTool.MissingEndpoint serverEndpoint)
        -- Get token from config - lookup server config to get token
        serverCfg <-
          Map.lookup serverName atticConfig.servers
            & maybeToRight (AtticTool.MissingEndpoint serverEndpoint)
        token <-
          serverCfg.token
            & maybeToRight (AtticTool.MissingToken $ AtticServer serverName serverEndpoint)
        -- Create the login process
        let server = AtticServer serverName serverEndpoint
        pure (Attic.atticLoginProcess server token, server) of
        Left err -> throwError $ atticErrorToPipelineError urlText serverEndpoint err
        Right result -> pure result

      -- Run attic login
      runProcesses env.workspaceDir env.outputLog logger (one loginProc) >>= \case
        Left err -> throwError $ PipelineTerminated err
        Right ExitSuccess -> logger Info "Attic login successful"
        Right (ExitFailure _code) -> throwError $ PipelineTerminated Terminated

      -- Push each result to cache
      results <- forM (toList paths) $ \resultPath -> do
        logger Info $ "Pushing " <> toText resultPath
        let pushProc = Attic.atticPushProcess server cacheName resultPath
        runProcesses env.workspaceDir env.outputLog logger (one pushProc)

      -- Return first failure or success
      case find isLeft results of
        Just (Left err) -> throwError $ PipelineTerminated err
        Just (Right _) -> error "impossible: find isLeft returned a Right"
        Nothing -> case find isFailure results of
          Just (Right code) -> pure code
          Just (Left _) -> error "impossible: find isFailure returned a Left after filtering"
          Nothing -> pure ExitSuccess
  where
    isFailure (Right ExitSuccess) = False
    isFailure (Right _) = True
    isFailure (Left _) = False

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
  PipelineEnv ->
  ViraPipeline ->
  (forall es1. (Log (RichMessage IO) :> es1, ER.Reader LogContext :> es1, IOE :> es1) => Severity -> Text -> Eff es1 ()) ->
  Eff es ExitCode
signoffImpl env pipeline logger = do
  if pipeline.signoff.enable
    then do
      logger Info "Creating commit signoff"
      let signoffProc = Signoff.create Signoff.Force statusTitle
          statusTitle = "vira/" <> toString nixSystem <> "/ci"
      result <- runProcesses env.workspaceDir env.outputLog logger (one signoffProc)
      case result of
        Left err -> throwError $ PipelineTerminated err
        Right exitCode -> pure exitCode
    else do
      logger Info "Signoff disabled, skipping"
      pure ExitSuccess
