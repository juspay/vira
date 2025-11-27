{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Signoff (
  performSignoff,
) where

import Prelude hiding (state)

import BB.Config (ServerConfig (token))
import Bitbucket.API.V1.BuildStatus (BuildState (..), BuildStatus (..), postBuildStatus)
import Bitbucket.API.V1.Core (ServerEndpoint (..))
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.Git.Platform (GitPlatform (..))
import Effectful.Git.Types (CommitID)
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import GH.Signoff qualified as GHSignoff
import Vira.CI.Error (ConfigurationError (..), PipelineError (..))
import Vira.CI.Pipeline.Effect (PipelineEnv (tools), logPipeline)
import Vira.CI.Pipeline.Process (runProcess)
import Vira.Environment.Tool.Tools.Bitbucket (mkBitbucketSuggestion)
import Vira.Environment.Tool.Type.ToolData (ToolData (status))
import Vira.Environment.Tool.Type.Tools (Tools (bitbucket))

{- | Create commit signoffs for a specific git platform.

Takes commit ID, platform, repository directory, and signoff names.
Handles platform-specific signoff creation and error handling.
-}
performSignoff ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  -- | Commit ID to sign off
  CommitID ->
  -- | Git platform (GitHub or Bitbucket)
  GitPlatform ->
  -- | Repository directory
  FilePath ->
  -- | Signoff names (e.g., "vira/x86_64-linux")
  NonEmpty String ->
  Eff es ()
performSignoff commitId platform repoDir signoffNames = do
  env <- ER.ask @PipelineEnv
  case platform of
    GitHub -> do
      logPipeline Info $ "Creating " <> show (length signoffNames) <> " GitHub signoffs: " <> show (toList signoffNames)
      let ghProc = GHSignoff.create GHSignoff.Force signoffNames
      runProcess repoDir ghProc
      logPipeline Info "All GitHub signoffs succeeded"
    Bitbucket bitbucketHost -> do
      logPipeline Info $ "Creating " <> show (length signoffNames) <> " Bitbucket signoffs: " <> show (toList signoffNames)
      let endpoint = ServerEndpoint bitbucketHost
          commitHash = toText commitId
          bitbucketUrl = "https://" <> bitbucketHost
          suggestion = mkBitbucketSuggestion bitbucketUrl
      -- Get server config from tools
      case env.tools.bitbucket.status of
        Left configErr -> do
          logPipeline Error $ "Failed to load Bitbucket config: " <> configErr
          logPipeline Info $ show @Text suggestion
          throwError $ PipelineConfigurationError (MalformedConfig configErr)
        Right servers -> case Map.lookup endpoint servers of
          Nothing -> do
            logPipeline Error $ "Server not configured: " <> show endpoint
            logPipeline Info $ show @Text suggestion
            throwError $ PipelineConfigurationError (MalformedConfig $ "Server not configured: " <> show endpoint)
          Just (serverConfig :: ServerConfig) -> do
            -- Create and post each signoff
            forM_ signoffNames $ \signoffName -> do
              let status =
                    BuildStatus
                      { state = Successful
                      , key = toText signoffName
                      , name = toText signoffName
                      , url = ""
                      , description = "Vira signoff"
                      }
              result <- postBuildStatus endpoint serverConfig.token commitHash status
              case result of
                Left httpEx -> do
                  let errMsg = "HTTP request failed: " <> show httpEx
                  logPipeline Error $ "Bitbucket signoff failed: " <> errMsg
                  throwError $ PipelineConfigurationError (MalformedConfig errMsg)
                Right _ -> pass
            logPipeline Info "All Bitbucket signoffs succeeded"
