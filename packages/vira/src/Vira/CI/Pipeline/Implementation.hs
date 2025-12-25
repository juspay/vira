{-# LANGUAGE DeriveAnyClass #-}
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
import Data.Aeson (FromJSON, eitherDecode, eitherDecodeFileStrict)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List qualified as List
import Data.Text qualified as T
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
import System.Nix.System (System (..))
import System.Process (proc, readProcess)
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Error (ConfigurationError (..), PipelineError (..), pipelineToolError)
import Vira.CI.Nix.RemoteBuilderResolver (BuildTarget (..), partitionByLocalSystem)
import Vira.CI.Pipeline.Effect
import Vira.CI.Pipeline.Process (runProcess, runProcessWithContext)
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

  -- Build each flake for each target
  forM pipeline.build.flakes $ \flake -> do
    -- Build each target for this flake
    targetResults <- forM buildTargets $ \target ->
      buildFlakeForTarget target flake
    -- Merge all target results by combining their devourResult fields
    case nonEmpty targetResults of
      Nothing -> throwError $ PipelineConfigurationError $ MalformedConfig "No build targets"
      Just rs -> do
        -- Combine all devourResults using Semigroup
        let firstResult = head rs
            mergedDevourResult = sconcat $ fmap (.devourResult) rs
        pure $ firstResult {devourResult = mergedDevourResult}

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
    logPipeline Info "Building locally"
    buildFlakeWithDevourFlakeCtx sys flake
  RemoteBuild sys builder -> do
    logPipeline Info $ "Building on remote: " <> builder.uri
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

-- | Build a single flake with build context for JSON log streaming
buildFlakeWithDevourFlakeCtx ::
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
  Flake ->
  Eff es BuildResult
buildFlakeWithDevourFlakeCtx sys (Flake flakePath overrideInputs) = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
      buildCtx = LogContext [("flake", toText flakePath), ("system", sys.unSystem)]
  let buildProc =
        proc nix $
          devourFlake $
            DevourFlakeArgs
              { flakePath = flakePath
              , systems = [sys]
              , outLink = Just (flakePath </> "result")
              , overrideInputs = overrideInputs
              }

  logPipeline Info $ "Building flake at " <> toText flakePath

  -- Run build process with build context for JSON streaming
  runProcessWithContext repoDir buildCtx buildProc

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
      buildCtx = LogContext [("flake", toText flakePath), ("system", sys.unSystem)]
      -- Extract host from ssh-ng://user@host or ssh://user@host
      sshHost =
        toString $
          fromMaybe sshUri $
            T.stripPrefix "ssh-ng://" sshUri <|> T.stripPrefix "ssh://" sshUri
      -- SSH options for non-interactive operation
      sshOpts = ["-o", "BatchMode=yes", "-o", "StrictHostKeyChecking=accept-new", "-o", "ConnectTimeout=30"]

  -- Step 1: Archive the flake to get its store path
  logPipeline Info "Archiving flake to store..."
  flakeStorePath <- liftIO $ do
    -- nix flake archive --json returns {"path": "/nix/store/...", ...}
    output <- readProcess nix ["flake", "archive", "--json", repoDir </> flakePath] ""
    case eitherDecode (LBS.pack output) of
      Right (result :: FlakeArchiveOutput) -> pure result.path
      Left err -> error $ toText $ "Failed to parse flake archive output: " <> err

  logPipeline Info $ "Flake store path: " <> toText flakeStorePath

  -- Step 2: Copy flake source to remote
  logPipeline Info $ "Copying flake to remote: " <> sshUri
  let copyToProc = proc nix ["copy", "--to", toString sshUri, "--no-check-sigs", flakeStorePath]
  runProcessWithContext repoDir buildCtx copyToProc

  -- Step 3: SSH and run devour-flake on the remote using the store path
  logPipeline Info "Building on remote via SSH..."
  let devourFlakeArgs =
        devourFlake $
          DevourFlakeArgs
            { flakePath = flakeStorePath -- Use store path, not relative path
            , systems = [sys]
            , outLink = Nothing -- No out-link on remote
            , overrideInputs = overrideInputs
            }
      -- Construct the remote nix build command
      remoteBuildCmd = List.unwords $ ["nix"] <> map escapeArg devourFlakeArgs
      sshBuildProc = proc "ssh" $ sshOpts <> [sshHost, remoteBuildCmd]
  runProcessWithContext repoDir buildCtx sshBuildProc

  -- Step 4: Run devour-flake locally to get the result JSON
  -- This will substitute the already-built outputs from the remote
  logPipeline Info "Evaluating result locally (substituting from remote)..."
  buildFlakeWithDevourFlakeCtx sys (Flake flakePath overrideInputs)
  where
    -- Escape shell arguments for SSH
    escapeArg :: String -> String
    escapeArg s = "'" <> concatMap escapeChar s <> "'"
    escapeChar '\'' = "'\\''"
    escapeChar c = [c]

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

-- | Output of @nix flake archive --json@
newtype FlakeArchiveOutput = FlakeArchiveOutput
  { path :: FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
