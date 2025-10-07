{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline (runPipeline, defaultPipeline, PipelineError (..)) where

import Attic qualified
import Attic.Config (lookupEndpointWithToken)
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Git qualified as Git
import Effectful.Process (CreateProcess)
import GH.Signoff qualified as Signoff
import Language.Haskell.Interpreter (InterpreterError (..))
import Shower qualified
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Info qualified as SysInfo
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Environment qualified as Env
import Vira.CI.Error
import Vira.CI.Pipeline.Type
import Vira.Lib.Omnix qualified as Omnix
import Vira.Lib.Process (alwaysUnderPath)
import Vira.State.Type (cloneUrl, headCommit)
import Vira.Supervisor.Task qualified as Task
import Vira.Tool.Core (ToolError (..))
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Type.ToolData (status)
import Vira.Tool.Type.Tools (attic)

-- | Run `ViraPipeline` for the given `ViraEnvironment`
runPipeline ::
  (Task.AppTaskStack es) =>
  ViraEnvironment ->
  Eff es (Either PipelineError ExitCode)
runPipeline env = do
  -- 1. Setup workspace and clone
  let setupProcs =
        one $ Git.cloneAtCommit env.repo.cloneUrl env.branch.headCommit Env.projectDirName
  Task.runProcesses setupProcs >>= \case
    Left err ->
      pure $ Left $ PipelineTerminated err
    Right ExitSuccess -> do
      -- 2. Configure the pipeline, looking for optional vira.hs
      runErrorNoCallStack @InterpreterError (environmentPipeline env Task.logToWorkspaceOutput) >>= \case
        Left err -> do
          pure $ Left $ PipelineConfigurationError $ InterpreterError err
        Right pipeline -> do
          Task.logToWorkspaceOutput $ toText $ "ℹ️ Pipeline configuration:\n" <> Shower.shower pipeline
          case pipelineProcesses env pipeline of
            Left err -> do
              pure $ Left err
            Right pipelineProcs -> do
              -- 3. Run the actual CI pipeline.
              Task.runProcesses pipelineProcs <&> first PipelineTerminated
    Right exitCode -> do
      pure $ Right exitCode

{- | Get all the processes to run as part of this Pipeline

TODO: To be rewritten during https://github.com/juspay/vira/issues/6
-}
pipelineProcesses :: ViraEnvironment -> ViraPipeline -> Either PipelineError (NonEmpty CreateProcess)
pipelineProcesses env pipeline = do
  (_, postBuildProcs) <- runWriterT $ do
    tell <=< lift $ cacheProcs env pipeline.cache
    tell $ signoffProcs pipeline.signoff
  let procs = buildProc pipeline.build :| postBuildProcs
  pure $ procs <&> alwaysUnderPath Env.projectDirName

buildProc :: BuildStage -> CreateProcess
buildProc stage =
  Omnix.omnixCiProcess (overrideInputsToArgs stage.overrideInputs)
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
        -- Get server name for endpoint (only if it has a token)
        serverName <-
          lookupEndpointWithToken atticConfig serverEndpoint
            & maybeToRight (AtticTool.MissingEndpoint serverEndpoint)
        -- Create the push process (token already validated by lookupEndpointWithToken)
        pure $ Attic.atticPushProcess (AtticServer serverName serverEndpoint) cacheName "result"
      pure $ one pushProc

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

signoffProcs :: SignoffStage -> [CreateProcess]
signoffProcs stage =
  [Signoff.create Signoff.Force statusTitle | stage.enable]
  where
    nixSystem = SysInfo.arch <> "-" <> SysInfo.os
    statusTitle = "vira/" <> nixSystem <> "/ci"

{- | Load pipeline configuration for the given project environment.

Uses vira.hs from project repository.
-}
environmentPipeline ::
  (Error InterpreterError :> es, IOE :> es) =>
  -- | Project's environment
  ViraEnvironment ->
  -- | Logger function
  (Text -> Eff es ()) ->
  Eff es ViraPipeline
environmentPipeline env logger = do
  let
    pipeline = defaultPipeline
    viraConfigPath = Env.projectDir env </> "vira.hs"
  configExists <- liftIO $ doesFileExist viraConfigPath

  if configExists
    then do
      logger "Found vira.hs configuration file, applying customizations..."
      content <- liftIO $ readFileBS viraConfigPath
      liftIO (Configuration.applyConfig (decodeUtf8 content) (Env.viraContext env) pipeline) >>= \case
        Left err -> do
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
    { build = BuildStage mempty
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    }
