{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Processes (
  pipelineProcesses,
) where

import Attic qualified
import Attic.Config (lookupEndpointWithToken)
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified
import Data.List.NonEmpty (appendList)
import DevourFlake (DevourFlakeArgs (..), devourFlake)
import Effectful.Process (CreateProcess)
import GH.Signoff qualified as Signoff
import System.FilePath ((</>))
import System.Nix.Core (nix)
import System.Nix.System (nixSystem)
import System.Process (proc)
import Vira.CI.Error (ConfigurationError (..), PipelineError (..))
import Vira.CI.Pipeline.Type
import Vira.Tool.Core (ToolError (..))
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Type.ToolData (status)
import Vira.Tool.Type.Tools (Tools, attic)

{- | Get all the processes to run as part of this Pipeline

TODO: To be rewritten during https://github.com/juspay/vira/issues/6
-}
pipelineProcesses :: Tools -> ViraPipeline -> Either PipelineError (NonEmpty CreateProcess)
pipelineProcesses tools pipeline = do
  postBuildProcs <- do
    cachePs <- cacheProcs tools pipeline.cache
    pure $ cachePs <> signoffProcs pipeline.signoff
  let bs = buildProcs pipeline.build.flakes
  pure $ bs `appendList` postBuildProcs

buildProcs :: NonEmpty Flake -> NonEmpty CreateProcess
buildProcs = fmap buildProc
  where
    buildProc :: Flake -> CreateProcess
    buildProc flake =
      proc nix $ devourFlake args
      where
        args =
          DevourFlakeArgs
            { flakePath = flake.path
            , systems = Nothing
            , outLink = Just (flake.path </> "result")
            , overrideInputs = flake.overrideInputs
            }

cacheProcs :: Tools -> CacheStage -> Either PipelineError [CreateProcess]
cacheProcs tools stage =
  go tools.attic stage.url
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
    statusTitle = "vira/" <> toString nixSystem <> "/ci"
