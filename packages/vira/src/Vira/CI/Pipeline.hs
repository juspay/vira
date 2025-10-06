{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline (runPipeline, defaultPipeline, PipelineError (..)) where

import Attic.Config (ConfigError)
import Attic.Types (AtticServerEndpoint)
import Attic.Url qualified
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Git qualified as Git
import Effectful.Process (CreateProcess (cwd))
import GH.Signoff qualified as Signoff
import Language.Haskell.Interpreter (InterpreterError)
import Optics.Core
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Info qualified as SysInfo
import Text.Show qualified as TS
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Environment (ViraEnvironment (..), projectDir, viraContext)
import Vira.CI.Pipeline.Type
import Vira.Lib.Omnix qualified as Omnix
import Vira.State.Type (branchName, cloneUrl, headCommit, name)
import Vira.Supervisor.Task qualified as Task
import Vira.Supervisor.Type (TaskException)
import Vira.Tool.Core (ToolError (..))
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Type.ToolData (status)
import Vira.Tool.Type.Tools (attic)

-- | Configuration error types
data ConfigurationError
  = InterpreterError InterpreterError
  | MalformedConfig Text
  deriving stock (Show)

-- | Pipeline-specific errors
data PipelineError
  = PipelineConfigurationError ConfigurationError
  | PipelineToolError ToolError
  | PipelineEmpty
  | PipelineTaskException TaskException

instance TS.Show PipelineError where
  show (PipelineToolError (ToolError msg)) = toString msg
  show (PipelineConfigurationError (InterpreterError err)) = TS.show err
  show (PipelineConfigurationError (MalformedConfig msg)) = toString msg
  show PipelineEmpty = "Pipeline is empty - no stages to run"
  show (PipelineTaskException err) = TS.show err

-- | Run `ViraPipeline` for the given `ViraEnvironment`
runPipeline ::
  (Task.AppTaskStack es) =>
  ViraEnvironment ->
  Eff es (Either PipelineError ExitCode)
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
        Left err -> do
          Task.logToWorkspaceOutput $ "Pipeline configuration failed: " <> show err
          pure $ Left $ PipelineConfigurationError $ InterpreterError err
        Right pipeline -> do
          Task.logToWorkspaceOutput $ "Pipeline: " <> show pipeline
          case pipelineToProcesses env pipeline of
            Left err -> do
              Task.logToWorkspaceOutput $ "Failed to create pipeline processes: " <> toText (TS.show err :: String)
              pure $ Left err
            Right pipelineProcs -> do
              Task.logToWorkspaceOutput $ "Running " <> show (length pipelineProcs) <> " pipeline stages..."
              Task.runProcesses pipelineProcs <&> first PipelineTaskException
    _ -> do
      pure $ setupResult & first PipelineTaskException

-- | Convert pipeline configuration to CreateProcess list
pipelineToProcesses :: ViraEnvironment -> ViraPipeline -> Either PipelineError (NonEmpty CreateProcess)
pipelineToProcesses env pipeline = do
  procs' <- pipelineToProcesses' env pipeline
  procs <- nonEmpty procs' & maybeToRight PipelineEmpty
  pure $ procs <&> \p -> p {cwd = Just (projectDir env)}

pipelineToProcesses' :: ViraEnvironment -> ViraPipeline -> Either PipelineError [CreateProcess]
pipelineToProcesses' env pipeline = do
  cachePs <- cacheProcs env pipeline.cache
  pure $
    concat
      [ buildProcs pipeline.build
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

cacheProcs :: ViraEnvironment -> CacheStage -> Either PipelineError [CreateProcess]
cacheProcs env stage =
  go env.tools.attic stage.url
  where
    go _ Nothing = pure []
    go attic (Just urlText) = do
      -- Parse cache URL once
      (serverEndpoint, cacheName) <-
        Attic.Url.parseCacheUrl urlText
          & first (parseErrorToPipelineError urlText)

      -- Get attic config and create push process
      pushProc <- first (atticErrorToPipelineError urlText serverEndpoint) $ do
        atticConfig <- attic.status
        AtticTool.createPushProcess atticConfig serverEndpoint cacheName "result"
      pure $ one pushProc

    parseErrorToPipelineError :: Text -> Attic.Url.ParseError -> PipelineError
    parseErrorToPipelineError url err =
      PipelineConfigurationError $
        MalformedConfig $
          "Invalid cache URL '" <> url <> "': " <> show err

    atticErrorToPipelineError :: Text -> AtticServerEndpoint -> ConfigError -> PipelineError
    atticErrorToPipelineError url endpoint err =
      PipelineToolError $ case AtticTool.configErrorToSuggestion (Just endpoint) err of
        Just suggestion ->
          ToolError $
            "Attic configuration error for cache URL '"
              <> url
              <> "': "
              <> show err
              <> "\n\nSuggestion: Run the following in your terminal\n\n"
              <> AtticTool.suggestionToText suggestion
        Nothing -> ToolError $ "Attic configuration error: " <> show err

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
    pipeline = defaultPipeline & hardcodePerRepoConfig env
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
defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage True mempty
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    }
