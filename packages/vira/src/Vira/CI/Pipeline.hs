{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline (runPipeline, defaultPipeline) where

import Attic
import Data.Dependent.Map qualified as DMap
import Data.Some (Some (Some))
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Git qualified as Git
import Effectful.Process (CreateProcess (cwd), env)
import GH.Signoff qualified as Signoff
import Language.Haskell.Interpreter (InterpreterError)
import Optics.Core
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Info qualified as SysInfo
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Environment (ViraEnvironment (..), projectDir, viraContext)
import Vira.CI.Pipeline.Type
import Vira.Lib.Cachix
import Vira.Lib.Omnix qualified as Omnix
import Vira.State.Type
import Vira.Supervisor.Task qualified as Task
import Vira.Supervisor.Type (TaskException (TaskFailed))
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Type (Tool (..), ToolData (..))
import Vira.Tool.Type qualified as Tool

-- | Pipeline-specific errors
data PipelineError
  = PipelineConfigurationError InterpreterError
  | PipelineToolError Tool.ToolError
  | PipelineEmpty
  deriving stock (Show)

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
        Left (PipelineConfigurationError -> err) -> do
          Task.logToWorkspaceOutput $ "Pipeline configuration failed: " <> show err
          pure $ Left $ TaskFailed $ "Pipeline configuration failed: " <> show err
        Right pipeline -> do
          Task.logToWorkspaceOutput $ "Pipeline: " <> show pipeline
          case pipelineToProcesses env pipeline of
            Left err -> do
              Task.logToWorkspaceOutput $ "Failed to create pipeline processes: " <> show err
              pure $ Left $ TaskFailed $ "Failed to create pipeline processes: " <> show err
            Right pipelineProcs -> do
              Task.logToWorkspaceOutput $ "Running " <> show (length pipelineProcs) <> " pipeline stages..."
              Task.runProcesses pipelineProcs
    _ -> do
      pure setupResult

-- | Convert pipeline configuration to CreateProcess list
pipelineToProcesses :: ViraEnvironment -> ViraPipeline -> Either PipelineError (NonEmpty CreateProcess)
pipelineToProcesses env pipeline = do
  procs <- pipelineToProcesses' env pipeline & first PipelineToolError
  case procs of
    [] -> Left PipelineEmpty
    (x : xs) -> Right $ (x :| xs) <&> \p -> p {cwd = Just (projectDir env)}

pipelineToProcesses' :: ViraEnvironment -> ViraPipeline -> Either Tool.ToolError [CreateProcess]
pipelineToProcesses' env pipeline = do
  atticPs <- atticProcs env pipeline.attic
  cachePs <- cacheProcs env pipeline.cache
  pure $
    concat
      [ buildProcs pipeline.build
      , atticPs
      , cachixProcs env pipeline.cachix
      , cachePs
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

atticProcs :: ViraEnvironment -> AtticStage -> Either Tool.ToolError [CreateProcess]
atticProcs env stage =
  pure $
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

cacheProcs :: ViraEnvironment -> CacheStage -> Either Tool.ToolError [CreateProcess]
cacheProcs env stage = case stage.url of
  Nothing -> Right []
  Just urlText -> do
    ToolData {info = atticConfigResult} <- DMap.lookup Attic env.tools & maybeToRight (Tool.ToolError (Some Attic) "Attic tool not found in cache")
    pushProc <- first (Tool.ToolError (Some Attic) . show) $ AtticTool.createPushProcess atticConfigResult urlText "result"
    pure $ one pushProc

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
