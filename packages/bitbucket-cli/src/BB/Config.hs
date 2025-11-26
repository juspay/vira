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
  ServerConfig (..),
  ConfigError (..),
) where

import Bitbucket.API.V1.Core (ServerEndpoint (..), Token (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
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
