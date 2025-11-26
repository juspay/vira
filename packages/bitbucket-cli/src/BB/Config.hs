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
  ConfigError (..),
) where

import Bitbucket.API.V1.Core (BitbucketConfig (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Network.HTTP.Req (useHttpsURI)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath (takeDirectory)
import Text.URI (mkURI)

-- | Configuration loading errors
data ConfigError
  = ConfigFileNotFound FilePath
  | JsonDecodeError Text
  | UrlParseError Text SomeException
  | InvalidUrlScheme Text
  deriving stock (Show)

{- | Get the Bitbucket config file path

Uses XDG Base Directory specification, typically:
- Linux/macOS: @$XDG_CONFIG_HOME/bb/config.json@ or @~/.config/bb/config.json@
- Windows: @%APPDATA%/bb/config.json@
-}
getConfigPath :: IO FilePath
getConfigPath = getXdgDirectory XdgConfig "bb/config.json"

-- | Configuration file structure
data Config = Config
  { baseUrl :: Text
  , token :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

{- | Load Bitbucket configuration from config file

Config file format (JSON):
@
{
  "baseUrl": "https://bitbucket.juspay.net",
  "token": "BBDC-xyz..."
}
@
-}
loadConfig :: IO (Either ConfigError BitbucketConfig)
loadConfig = do
  configPath <- getConfigPath
  exists <- doesFileExist configPath
  if not exists
    then pure $ Left $ ConfigFileNotFound configPath
    else do
      contentBS <- readFileBS configPath
      case Aeson.eitherDecodeStrict contentBS of
        Left err -> pure $ Left $ JsonDecodeError $ toText err
        Right config -> pure $ parseConfig config

-- | Parse config from JSON structure
parseConfig :: Config -> Either ConfigError BitbucketConfig
parseConfig config = do
  let baseUrlText = config.baseUrl
      tokenText = config.token

  -- Parse URL using text-uri and req
  uri <- case mkURI baseUrlText of
    Left parseErr -> Left $ UrlParseError baseUrlText parseErr
    Right u -> Right u

  baseUrl <- case useHttpsURI uri of
    Just (url, _) -> Right url
    Nothing -> Left $ InvalidUrlScheme baseUrlText

  pure $ BitbucketConfig {baseUrl = baseUrl, token = tokenText}

{- | Save configuration to config file

Writes config to ~/.config/bb/config.json, creating directories if needed.
-}
saveConfig :: Text -> Text -> IO ()
saveConfig baseUrlText tokenText = do
  configPath <- getConfigPath
  createDirectoryIfMissing True (takeDirectory configPath)
  let config = Config {baseUrl = baseUrlText, token = tokenText}
  writeFileLBS configPath $ Aeson.encode config
