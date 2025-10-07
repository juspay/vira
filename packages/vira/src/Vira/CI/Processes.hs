{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Processes (
  pipelineProcesses,
) where

import Attic qualified
import Attic.Config (lookupEndpointWithToken)
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT (..))
import Effectful.Process (CreateProcess)
import GH.Signoff qualified as Signoff
import System.Info qualified as SysInfo
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Environment qualified as Env
import Vira.CI.Error (ConfigurationError (..), PipelineError (..))
import Vira.CI.Pipeline.Type
import Vira.Lib.Omnix qualified as Omnix
import Vira.Lib.Process (alwaysUnderPath)
import Vira.Tool.Core (ToolError (..))
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Type.ToolData (status)
import Vira.Tool.Type.Tools (attic)

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
