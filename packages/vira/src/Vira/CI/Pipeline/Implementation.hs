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
import DevourFlake.Result (extractSystems)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Environment (Environment)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.Git.Command.Clone qualified as Git
import Effectful.Git.Platform (detectPlatform)
import Effectful.Git.Types (Commit (id))
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import Shower qualified
import System.FilePath ((</>))
import System.Nix.Config.Builders (RemoteBuilder (..))
import System.Nix.Core (nix)
import System.Nix.System (System)
import System.Process (proc)
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Error (ConfigurationError (..), PipelineError (..), pipelineToolError)
import Vira.CI.Nix.RemoteBuilderResolver (BuildTarget (..), partitionByLocalSystem)
import Vira.CI.Pipeline.Effect
import Vira.CI.Pipeline.Process (runProcess)
import Vira.CI.Pipeline.Signoff qualified as Signoff
import Vira.CI.Pipeline.Type (BuildStage (..), CacheStage (..), Flake (..), SignoffStage (..), ViraPipeline (..))
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
          LoadConfig -> loadConfigImpl
          Build pipeline -> buildImpl pipeline
          Cache pipeline buildResults -> cacheImpl pipeline buildResults
          Signoff pipeline buildResults -> signoffImpl pipeline buildResults
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
  let projectDirName = "project"
  cloneProc <-
    Git.cloneAtCommit
      repo.cloneUrl
      branch.headCommit.id
      projectDirName

  logPipeline Info $ "Cloning repository at commit " <> toText branch.headCommit.id

  runProcess workspacePath cloneProc

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
  Eff es ViraPipeline
loadConfigImpl = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
      viraConfigPath = repoDir </> "vira.hs"
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
    -- When onlyBuild is enabled, restrict to current system and disable cache/signoff
    patchPipelineForCli :: ViraContext -> ViraPipeline -> ViraPipeline
    patchPipelineForCli ctx pipeline
      | ctx.onlyBuild =
          pipeline
            { -- Don't signoff when only building
              signoff = pipeline.signoff {enable = False}
            , -- Don't push to cache when only building
              cache = pipeline.cache {url = Nothing}
            , -- Only build for current system when only building
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
  ViraPipeline ->
  Eff es (NonEmpty BuildResult)
buildImpl pipeline = do
  let systems = pipeline.build.systems
  logPipeline Info $ "Building " <> show (length pipeline.build.flakes) <> " flakes"
  if length systems <= 1
    then do
      -- Single system or default: use existing devour-flake path
      forM pipeline.build.flakes $ \flake ->
        buildFlakeWithDevourFlake systems flake
    else do
      -- Multi-platform: partition into local vs remote and build in parallel
      logPipeline Info $ "Multi-platform build: " <> show (length systems) <> " systems"
      buildMultiPlatform pipeline

-- | Multi-platform build: partitions systems into local and remote, builds in parallel
buildMultiPlatform ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  ViraPipeline ->
  Eff es (NonEmpty BuildResult)
buildMultiPlatform pipeline = do
  -- Partition systems into local vs remote build targets
  -- Adapt Text errors to PipelineError
  buildTargetsResult <-
    runErrorNoCallStack $
      partitionByLocalSystem pipeline.build.systems
  buildTargets <- case buildTargetsResult of
    Left msg -> throwError $ PipelineConfigurationError $ MalformedConfig msg
    Right targets -> pure targets
  logPipeline Info $ "Build targets: " <> show buildTargets

  -- Build each flake for each target in parallel
  -- For now, build flakes sequentially but targets could be parallelized
  -- TODO: Use pooledMapConcurrently for true parallelism when ready
  forM pipeline.build.flakes $ \flake -> do
    -- Build each target for this flake
    targetResults <- forM buildTargets $ \target ->
      buildFlakeForTarget target flake
    -- Return the first result (they should all produce equivalent results)
    -- In future, we could merge the devour-flake results
    case nonEmpty targetResults of
      Nothing -> throwError $ PipelineConfigurationError $ MalformedConfig "No build targets"
      Just rs -> pure $ head rs

-- | Build a flake for a specific target (local or remote)
buildFlakeForTarget ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  BuildTarget ->
  Flake ->
  Eff es BuildResult
buildFlakeForTarget target flake = case target of
  LocalBuild sys -> do
    logPipeline Info $ "[" <> show sys <> "] Building locally"
    buildFlakeWithDevourFlake [sys] flake
  RemoteBuild sys builder -> do
    logPipeline Info $ "[" <> show sys <> "] Building on remote: " <> builder.uri
    buildFlakeOnRemote sys builder flake

-- | Build a single flake using devour-flake (local build)
buildFlakeWithDevourFlake ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  [System] ->
  Flake ->
  Eff es BuildResult
buildFlakeWithDevourFlake systems (Flake flakePath overrideInputs) = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
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
  runProcess repoDir buildProc

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

-- | Build a single flake on a remote builder via SSH
buildFlakeOnRemote ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  System ->
  RemoteBuilder ->
  Flake ->
  Eff es BuildResult
buildFlakeOnRemote sys builder (Flake flakePath overrideInputs) = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
      sshUri = builder.uri
      -- Build the flake reference with system filter
      flakeRef = "." <> (if null flakePath || flakePath == "." then "" else "/" <> flakePath)
      -- SSH options for non-interactive Nix operations
      sshOpts = "ssh -o BatchMode=yes -o StrictHostKeyChecking=accept-new -o ConnectTimeout=30"

  -- Use nix build with --store to build on remote
  -- This evaluates locally but performs the build on the remote store
  logPipeline Info $ "[" <> show sys <> "] Building on remote store: " <> sshUri
  let buildProc =
        proc nix $
          [ "build"
          , flakeRef
          , "--store"
          , toString sshUri
          , "--eval-store"
          , "auto" -- Evaluate locally
          , "--no-link"
          , "--json"
          ]
            <> ["--option", "ssh-options", sshOpts]
            <> concatMap (\(k, v) -> ["--override-input", toString k, toString v]) overrideInputs
  runProcess repoDir buildProc

  -- Copy the built outputs back to local store
  logPipeline Info $ "[" <> show sys <> "] Copying results from remote..."
  let copyFromProc =
        proc nix $
          ["copy", "--from", toString sshUri, "--no-check-sigs", flakeRef]
            <> ["--option", "ssh-options", sshOpts]
  runProcess repoDir copyFromProc

  -- Run devour-flake locally to get the result JSON (should be instant now)
  logPipeline Info $ "[" <> show sys <> "] Evaluating result locally..."
  buildFlakeWithDevourFlake [sys] (Flake flakePath overrideInputs)

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
  ViraPipeline ->
  NonEmpty BuildResult ->
  Eff es ()
cacheImpl pipeline buildResults = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
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

      -- Push to cache - paths are relative to repoDir
      let pathsToPush = fmap (.resultPath) buildResults
      logPipeline Info $ "Pushing " <> show (length pathsToPush) <> " result files: " <> show (toList pathsToPush)
      let pushProc = Attic.atticPushProcess server cacheName pathsToPush
      runProcess repoDir pushProc
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
          msg = "Attic configuration error for cache URL '" <> url <> "': " <> show err
       in pipelineToolError msg (Just suggestion)

-- | Implementation: Create signoff (one per system)
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
  ViraPipeline ->
  NonEmpty BuildResult ->
  Eff es ()
signoffImpl pipeline buildResults = do
  env <- ER.ask @PipelineEnv
  let commitId = env.viraContext.commitId
      cloneUrl = env.viraContext.cloneUrl
      repoDir = env.viraContext.repoDir
  if pipeline.signoff.enable
    then do
      -- Extract unique systems from all build results
      let systems = extractSystems $ fmap (.devourResult) (toList buildResults)
          signoffNames = fmap (\system -> "vira/" <> toString system) (toList systems)
      case nonEmpty signoffNames of
        Nothing -> throwError $ DevourFlakeMalformedOutput "build results" "No systems found in build results"
        Just names -> do
          -- Detect platform based on clone URL
          case detectPlatform cloneUrl of
            Nothing ->
              throwError $
                pipelineToolError
                  ("Signoff enabled but could not detect platform from clone URL: " <> cloneUrl <> ". Must be GitHub or Bitbucket.")
                  (Nothing :: Maybe Text)
            Just platform -> do
              Signoff.performSignoff commitId platform repoDir names
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
