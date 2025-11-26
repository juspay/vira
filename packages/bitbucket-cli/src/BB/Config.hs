{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Bitbucket configuration management

Reads configuration from ~/.config/bb/config.json file.
Follows XDG Base Directory specification for config file location.
-}
module BB.Config (
  loadConfig,
  saveConfig,
  getConfigPath,
  lookupServer,
  testConnection,
  ServerConfig (..),
  ConfigError (..),
) where

import Bitbucket.API.V1.Core (ServerEndpoint (..), Token (..), makeUrl)
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Control.Exception (catch)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log, withLogContext)
import Effectful.Reader.Static qualified as ER
import Network.HTTP.Req (
  GET (GET),
  HttpException,
  NoReqBody (NoReqBody),
  defaultHttpConfig,
  header,
  ignoreResponse,
  renderUrl,
  req,
  runReq,
  (/:),
 )
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath (takeDirectory)

-- | Server-specific configuration
newtype ServerConfig = ServerConfig
  { token :: Token
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Configuration loading errors
data ConfigError
  = ConfigFileNotFound FilePath
  | JsonDecodeError Text
  deriving stock (Show)

{- | Get the Bitbucket config file path

Uses XDG Base Directory specification, typically:
- Linux/macOS: @$XDG_CONFIG_HOME/bb/config.json@ or @~/.config/bb/config.json@
- Windows: @%APPDATA%/bb/config.json@
-}
getConfigPath :: IO FilePath
getConfigPath = getXdgDirectory XdgConfig "bb/config.json"

-- | Configuration file structure
newtype Config = Config
  { servers :: Map ServerEndpoint ServerConfig
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

{- | Load Bitbucket configuration from config file

Config file format (JSON):
@
{
  "servers": {
    "bitbucket.juspay.net": {
      "token": "BBDC-xyz..."
    },
    "bitbucket.example.com": {
      "token": "BBDC-abc..."
    }
  }
}
@
-}
loadConfig :: IO (Either ConfigError (Map ServerEndpoint ServerConfig))
loadConfig = do
  configPath <- getConfigPath
  exists <- doesFileExist configPath
  if not exists
    then pure $ Left $ ConfigFileNotFound configPath
    else do
      contentBS <- readFileBS configPath
      case Aeson.eitherDecodeStrict @Config contentBS of
        Left err -> pure $ Left $ JsonDecodeError $ toText err
        Right config -> pure $ Right config.servers

-- | Lookup server configuration by endpoint
lookupServer :: ServerEndpoint -> Map ServerEndpoint ServerConfig -> Maybe ServerConfig
lookupServer = Map.lookup

{- | Save configuration to config file

Writes config to ~/.config/bb/config.json, creating directories if needed.
-}
saveConfig :: Map ServerEndpoint ServerConfig -> IO ()
saveConfig servers = do
  configPath <- getConfigPath
  createDirectoryIfMissing True (takeDirectory configPath)
  let config = Config {servers}
  writeFileLBS configPath $ Aeson.encode config

{- | Test connection to Bitbucket API

Makes a simple API request to verify:
1. URL is reachable
2. API responds
3. Token is accepted
-}
testConnection :: (Log (RichMessage IO) :> es, ER.Reader LogContext :> es, IOE :> es) => ServerEndpoint -> Token -> Eff es (Either Text ())
testConnection endpoint (Token tok) = withLogContext [("api", "bitbucket")] $ do
  let baseUrl = makeUrl endpoint
      url = baseUrl /: "rest" /: "api" /: "1.0" /: "projects"
  log Debug $ "GET " <> renderUrl url
  liftIO $
    catch
      ( runReq defaultHttpConfig $ do
          let authHeader = encodeUtf8 $ "Bearer " <> tok
          void $
            req
              GET
              url
              NoReqBody
              ignoreResponse
              (header "Authorization" authHeader)
          pure (Right ())
      )
      (\(e :: HttpException) -> pure $ Left $ "HTTP error: " <> show e)
