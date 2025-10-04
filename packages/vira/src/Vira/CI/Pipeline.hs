{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline (runPipeline, defaultPipeline) where

import Attic
import Attic.Config qualified
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Git qualified as Git
import Effectful.Process (CreateProcess (cwd), env, proc)
import GH.Signoff qualified as Signoff
import Language.Haskell.Interpreter (InterpreterError)
import Optics.Core
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Info qualified as SysInfo
import Text.URI qualified as URI
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Environment (ViraEnvironment (..), projectDir, viraContext)
import Vira.CI.Pipeline.Type
import Vira.Lib.Cachix
import Vira.Lib.Omnix qualified as Omnix
import Vira.State.Type
import Vira.Supervisor.Task qualified as Task
import Vira.Supervisor.Type (TaskException (ConfigurationError))

-- | Run `ViraPipeline` for the given `ViraEnvironment`
runPipeline ::
  (Task.AppTaskStack es) =>
  ViraEnvironment ->
  Eff es (Either TaskException ExitCode)
runPipeline env = do
  -- 1. Setup workspace and clone
  -- HACK: We hardcoding "project" (see projectDir function in Environment.hs)
  let setupProcs =
        one $ Git.cloneAtCommit env.repo.cloneUrl env.branch.headCommit "project"
  setupResult <- Task.runProcesses setupProcs
  case setupResult of
    Right ExitSuccess -> do
      -- 2. Configure and run pipeline
      Task.logToWorkspaceOutput "Setting up pipeline..."
      runErrorNoCallStack @InterpreterError (pipelineForProject env Task.logToWorkspaceOutput) >>= \case
        Left interpreterError -> do
          Task.logToWorkspaceOutput $ "Pipeline configuration failed: " <> show interpreterError
          pure $ Left $ ConfigurationError interpreterError
        Right pipeline -> do
          Task.logToWorkspaceOutput $ "Pipeline: " <> show pipeline
          let pipelineProcs = pipelineToProcesses env pipeline
          Task.runProcesses pipelineProcs
    _ -> do
      pure setupResult

-- | Convert pipeline configuration to CreateProcess list
pipelineToProcesses :: ViraEnvironment -> ViraPipeline -> NonEmpty CreateProcess
pipelineToProcesses env pipeline =
  case pipelineToProcesses' env pipeline of
    [] -> proc "echo" ["No pipeline stages enabled"] :| []
    (x : xs) -> (x :| xs) <&> \p -> p {cwd = Just (projectDir env)}

pipelineToProcesses' :: ViraEnvironment -> ViraPipeline -> [CreateProcess]
pipelineToProcesses' env pipeline =
  concat
    [ buildProcs pipeline.build
    , atticProcs env pipeline.attic
    , cachixProcs env pipeline.cachix
    , cacheProcs pipeline.cache
    , signoffProcs pipeline.signoff
    ]

buildProcs :: BuildStage -> [CreateProcess]
buildProcs stage =
  [Omnix.omnixCiProcess (overrideInputsToArgs stage.overrideInputs) | stage.enable]
  where
    -- Convert override inputs to command line arguments
    overrideInputsToArgs :: [(Text, Text)] -> [String]
    overrideInputsToArgs =
      concatMap (\(key, value) -> ["--override-input", toString key, toString value])

atticProcs :: ViraEnvironment -> AtticStage -> [CreateProcess]
atticProcs env stage =
  if stage.enable
    then flip concatMap env.atticSettings $ \attic ->
      [ atticLoginProcess attic.atticServer attic.atticToken
      , atticPushProcess attic.atticServer attic.atticCacheName "result"
      ]
    else []

cachixProcs :: ViraEnvironment -> CachixStage -> [CreateProcess]
cachixProcs env stage =
  if stage.enable
    then flip concatMap env.cachixSettings $ \cachix ->
      [ cachixPushProcess cachix.cachixName "result" & \p ->
          p {env = Just [("CACHIX_AUTH_TOKEN", toString cachix.authToken)]}
      ]
    else []

cacheProcs :: CacheStage -> [CreateProcess]
cacheProcs stage = case stage.url of
  Nothing -> []
  Just urlText ->
    -- Parse URL text to extract server endpoint and cache name using modern-uri
    case parseCacheUrl urlText of
      Left err -> [proc "sh" ["-c", "echo 'Error: " <> err <> "' && exit 1"]]
      Right (serverEndpoint, cacheName) ->
        -- Validate attic config exists
        case validateAtticConfigIO serverEndpoint of
          Left err -> [proc "sh" ["-c", "echo 'Error: " <> err <> "' && exit 1"]]
          Right (serverName, _) ->
            -- Return attic push process
            [atticPushProcess (AtticServer serverName serverEndpoint) (AtticCache cacheName) "result"]
  where
    -- Parse cache URL like "https://cache.nixos.asia/oss" to extract server and cache name
    parseCacheUrl :: Text -> Either String (Text, Text)
    parseCacheUrl urlText = do
      -- Parse using modern-uri
      uri <- first show $ URI.mkURI urlText
      -- Extract path segments
      pathParts <- case URI.uriPath uri of
        Nothing -> Left "Cache URL must have a path"
        Just (_isAbsolute, pathSegments) ->
          let segments = map URI.unRText (toList pathSegments)
              nonEmptySegments = filter (not . T.null) segments
           in case nonEmpty nonEmptySegments of
                Nothing -> Left "Cache URL must have a path segment for the cache name"
                Just ne -> Right ne
      let cacheName' = last pathParts
      -- Render server endpoint without the path
      let serverEndpoint = URI.render $ uri {URI.uriPath = Nothing}
      Right (serverEndpoint, cacheName')

    -- Validate that attic config exists and has matching server
    validateAtticConfigIO :: Text -> Either String (Text, Attic.Config.AtticServerConfig)
    validateAtticConfigIO serverEndpoint = unsafePerformIO $ do
      atticConfigResult <- Attic.Config.readAtticConfig
      pure $ case atticConfigResult of
        Left _tomlErr -> Left "Attic not configured. Please run 'attic login <server-name> <server-url> <token>' first"
        Right Nothing -> Left "Attic not configured. Please run 'attic login <server-name> <server-url> <token>' first"
        Right (Just config) ->
          -- Find matching server by endpoint in the Map
          case find (\(_name, serverCfg) -> serverCfg.endpoint == serverEndpoint) (Map.toList config.servers) of
            Nothing -> Left $ "No attic server configured for endpoint: " <> toString serverEndpoint <> ". Please run 'attic login' first"
            Just (serverName, serverCfg) -> Right (serverName, serverCfg)

signoffProcs :: SignoffStage -> [CreateProcess]
signoffProcs stage =
  [Signoff.create Signoff.Force statusTitle | stage.enable]
  where
    nixSystem = SysInfo.arch <> "-" <> SysInfo.os
    statusTitle = "vira/" <> nixSystem <> "/ci"

-- HACK: Hardcoding until we have per-repo configuration
-- Until we have https://github.com/juspay/vira/issues/59
hardcodePerRepoConfig :: ViraEnvironment -> ViraPipeline -> ViraPipeline
hardcodePerRepoConfig env pipeline =
  case toString env.repo.name of
    "euler-lsp" ->
      eulerLspConfiguration env pipeline
    _ -> pipeline

eulerLspConfiguration :: ViraEnvironment -> ViraPipeline -> ViraPipeline
eulerLspConfiguration env pipeline =
  let
    isReleaseBranch = toString env.branch.branchName `isPrefixOf` "release-"
   in
    pipeline
      & #build
      % #overrideInputs
      .~ [("flake/local", "github:boolean-option/false") | isReleaseBranch]

-- | Load pipeline configuration for a project directory
pipelineForProject ::
  (Error InterpreterError :> es, IOE :> es) =>
  -- | Project's environment
  ViraEnvironment ->
  -- | Logger function
  (Text -> Eff es ()) ->
  Eff es ViraPipeline
pipelineForProject env logger = do
  let
    pipeline = defaultPipeline env & hardcodePerRepoConfig env
    viraConfigPath = projectDir env </> "vira.hs"
  configExists <- liftIO $ doesFileExist viraConfigPath

  if configExists
    then do
      logger "Found vira.hs configuration file, applying customizations..."
      content <- liftIO $ readFileBS viraConfigPath
      liftIO (Configuration.applyConfig (decodeUtf8 content) (viraContext env) pipeline) >>= \case
        Left err -> do
          logger $ "Failed to parse vira.hs: " <> show err
          throwError err
        Right customPipeline -> do
          logger "Successfully applied vira.hs configuration"
          pure customPipeline
    else do
      logger "No vira.hs found - using default pipeline"
      pure pipeline

-- | Create a default pipeline configuration
defaultPipeline :: ViraEnvironment -> ViraPipeline
defaultPipeline env =
  ViraPipeline
    { build = BuildStage True mempty
    , attic = AtticStage (isJust env.atticSettings)
    , cachix = CachixStage (isJust env.cachixSettings)
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    }
