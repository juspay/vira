{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Implementation (
  runPipeline,

  -- * Used in tests
  defaultPipeline,
) where

import Prelude hiding (asks, id)

import Attic qualified
import Attic.Config (lookupEndpointWithToken)
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Aeson (eitherDecodeFileStrict)
import DevourFlake (DevourFlakeArgs (..), devourFlake)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Environment (Environment)
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.Git.Command.Clone qualified as Git
import Effectful.Git.Types (Commit (..))
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import GH.Signoff qualified as Signoff
import Shower qualified
import System.FilePath ((</>))
import System.Nix.Core (nix)
import System.Nix.System (System)
import System.Process (proc)
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Error (ConfigurationError (..), PipelineError (..))
import Vira.CI.Pipeline.Effect
import Vira.CI.Pipeline.Process (runProcess)
import Vira.CI.Pipeline.Type (BuildStage (..), CacheStage (..), Flake (..), SignoffStage (..), ViraPipeline (..))
import Vira.Environment.Tool.Core (ToolError (..))
import Vira.Environment.Tool.Tools.Attic qualified as AtticTool
import Vira.Environment.Tool.Type.ToolData (status)
import Vira.Environment.Tool.Type.Tools (attic)
import Vira.State.Type (Branch (..), Repo (..))

-- | Run the unified Pipeline effect
runPipeline ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  , Environment :> es
  ) =>
  PipelineEnv ->
  Eff (Pipeline : ER.Reader PipelineEnv : es) a ->
  Eff es a
runPipeline env program =
  ER.runReader env $
    interpret
      ( \_ -> \case
          Clone repo branch workspacePath -> cloneImpl repo branch workspacePath
          LoadConfig repoDir -> loadConfigImpl repoDir
          Build repoDir pipeline -> buildImpl repoDir pipeline
          Cache repoDir pipeline buildResults -> cacheImpl repoDir pipeline buildResults
          Signoff repoDir pipeline -> signoffImpl repoDir pipeline
      )
      program

-- | Implementation: Clone repository
cloneImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  , Environment :> es
  ) =>
  Repo ->
  Branch ->
  FilePath ->
  Eff es FilePath
cloneImpl repo branch workspacePath = do
  env <- ER.ask @PipelineEnv
  let projectDirName = "project"
  cloneProc <-
    Git.cloneAtCommit
      repo.cloneUrl
      branch.headCommit.id
      projectDirName

  logPipeline Info $ "Cloning repository at commit " <> toText branch.headCommit.id

  runProcess workspacePath env.outputLog cloneProc

  let clonedDir = workspacePath </> projectDirName
  logPipeline Info $ "Repository cloned to " <> toText clonedDir
  pure clonedDir

-- | Implementation: Load vira.hs configuration
loadConfigImpl ::
  ( FileSystem :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  Eff es ViraPipeline
loadConfigImpl repoDir = do
  env <- ER.ask @PipelineEnv
  let viraConfigPath = repoDir </> "vira.hs"
  doesFileExist viraConfigPath >>= \case
    True -> do
      logPipeline Info "Found vira.hs configuration file, applying customizations..."
      content <- liftIO $ decodeUtf8 <$> readFileBS viraConfigPath
      Configuration.applyConfig content env.viraContext defaultPipeline >>= \case
        Left err -> throwError $ PipelineConfigurationError $ InterpreterError err
        Right p -> do
          logPipeline Info "Successfully applied vira.hs configuration"
          pure $ patchPipelineForCli env.viraContext p
    False -> do
      logPipeline Info "No vira.hs found - using default pipeline"
      pure $ patchPipelineForCli env.viraContext defaultPipeline
  where
    -- When running in CLI mode, restrict to current system and disable cache/signoff
    patchPipelineForCli :: ViraContext -> ViraPipeline -> ViraPipeline
    patchPipelineForCli ctx pipeline
      | ctx.cli =
          pipeline
            { -- Don't signoff when running in CLI
              signoff = pipeline.signoff {enable = False}
            , -- Don't push to cache when running in CLI
              cache = pipeline.cache {url = Nothing}
            , -- Only build for current system in CLI mode
              build = BuildStage {flakes = pipeline.build.flakes, systems = []}
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
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  ViraPipeline ->
  Eff es (NonEmpty BuildResult)
buildImpl repoDir pipeline = do
  logPipeline Info $ "Building " <> show (length pipeline.build.flakes) <> " flakes"
  -- Build each flake sequentially and return BuildResult for each
  forM pipeline.build.flakes $ \flake ->
    buildFlake repoDir pipeline.build.systems flake

-- | Build a single flake
buildFlake ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  [System] ->
  Flake ->
  Eff es BuildResult
buildFlake repoDir systems (Flake flakePath overrideInputs) = do
  env <- ER.ask @PipelineEnv
  let buildProc =
        proc nix $
          devourFlake $
            DevourFlakeArgs
              { flakePath = flakePath
              , systems
              , outLink = Just (flakePath </> "result")
              , overrideInputs = overrideInputs
              }

  logPipeline Info $ "Building flake at " <> toText flakePath

  -- Run build process from working directory
  runProcess repoDir env.outputLog buildProc

  -- Return relative path to result symlink (relative to repo root)
  let resultPath = flakePath </> "result"
  logPipeline Info $ "Build succeeded, result at " <> toText resultPath

  -- Parse the JSON result
  devourResult <- liftIO $ eitherDecodeFileStrict $ repoDir </> resultPath
  case devourResult of
    Left err ->
      throwError $ DevourFlakeMalformedOutput resultPath err
    Right parsed -> do
      logPipeline Info $ toText $ "Build result for " <> flakePath <> ":\n" <> Shower.shower parsed
      pure $ BuildResult flakePath resultPath parsed

-- | Implementation: Push to cache
cacheImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  ViraPipeline ->
  NonEmpty BuildResult ->
  Eff es ()
cacheImpl repoDir pipeline buildResults = do
  env <- ER.ask @PipelineEnv
  case pipeline.cache.url of
    Nothing -> do
      logPipeline Warning "Cache disabled, skipping"
    Just urlText -> do
      logPipeline Info $ "Pushing " <> show (length buildResults) <> " build results to cache"

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

      -- Extract paths to push (just the result symlinks)
      let pathsToPush = fmap (.resultPath) buildResults

      -- Push to cache - paths are relative to repoDir
      logPipeline Info $ "Pushing " <> show (length pathsToPush) <> " result files: " <> show (toList pathsToPush)
      let pushProc = Attic.atticPushProcess server cacheName pathsToPush
      runProcess repoDir env.outputLog pushProc
      logPipeline Info "Cache push succeeded"
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
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  FilePath ->
  ViraPipeline ->
  Eff es ()
signoffImpl repoDir pipeline = do
  env <- ER.ask @PipelineEnv
  if pipeline.signoff.enable
    then do
      logPipeline Info "Creating commit signoff"
      let signoffProc = Signoff.create Signoff.Force "vira"
      runProcess repoDir env.outputLog signoffProc
      logPipeline Info "Signoff succeeded"
    else
      logPipeline Warning "Signoff disabled, skipping"

-- | Default pipeline configuration
defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage {flakes = one defaultFlake, systems = []}
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    }
  where
    defaultFlake = Flake "." mempty
