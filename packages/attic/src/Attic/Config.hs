{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic configuration file parsing
module Attic.Config (
  AtticConfig (..),
  AtticServerConfig (..),
  ConfigError (..),
  getAtticConfig,
  readAtticConfig,
  TOML.TOMLError,
) where

import Attic.Types (AtticServerEndpoint (..), AtticToken (..))
import Data.Map.Strict qualified as Map
import System.Directory (XdgDirectory (..), doesFileExist, getXdgDirectory)
import TOML (DecodeTOML, TOMLError, getField, getFields)
import TOML qualified

-- | Attic server configuration from config.toml
data AtticServerConfig = AtticServerConfig
  { endpoint :: AtticServerEndpoint
  , token :: Maybe AtticToken
  }
  deriving stock (Eq, Show, Generic)

instance DecodeTOML AtticServerConfig where
  tomlDecoder =
    AtticServerConfig
      <$> getField "endpoint"
      <*> (getFields ["token"] <|> pure Nothing)

-- | Attic configuration from ~/.config/attic/config.toml
data AtticConfig = AtticConfig
  { defaultServer :: Maybe Text
  , servers :: Map Text AtticServerConfig
  }
  deriving stock (Eq, Show, Generic)

instance DecodeTOML AtticConfig where
  tomlDecoder =
    AtticConfig
      <$> (getFields ["default-server"] <|> pure Nothing)
      <*> getField "servers"

-- | Configuration errors
data ConfigError
  = -- | TOML configuration parse error
    ParseError TOMLError
  | -- | Attic is not configured
    NotConfigured
  | -- | No server configured for endpoint
    NoServerForEndpoint AtticServerEndpoint
  | -- | Server configured but no authentication token
    NoToken Text
  deriving stock (Show, Eq)

{- | Get validated Attic configuration

Combines reading and validation into a single operation.
Returns:
- Left ConfigError if config is missing, invalid, or incomplete
- Right AtticConfig if successfully parsed and validated
-}
getAtticConfig :: IO (Either ConfigError AtticConfig)
getAtticConfig = validateConfig <$> readAtticConfig
  where
    validateConfig :: Either TOMLError (Maybe AtticConfig) -> Either ConfigError AtticConfig
    validateConfig result = do
      mConfig <- first ParseError result
      config <- mConfig & maybeToRight NotConfigured

      -- Check if any server is missing a token
      case find (\(_, serverCfg) -> isNothing serverCfg.token) (Map.toList config.servers) of
        Just (serverName, _) -> Left (NoToken serverName)
        Nothing -> Right config

{- | Read Attic configuration from ~/.config/attic/config.toml

Returns:
- Left TOMLError if config file exists but failed to parse
- Right Nothing if config file doesn't exist (not configured)
- Right (Just config) if successfully parsed
-}
readAtticConfig :: IO (Either TOMLError (Maybe AtticConfig))
readAtticConfig = do
  configPath <- getXdgDirectory XdgConfig "attic/config.toml"
  exists <- doesFileExist configPath
  if not exists
    then pure $ Right Nothing
    else do
      contents <- readFileBS configPath <&> decodeUtf8
      case TOML.decode contents of
        Left err -> pure $ Left err
        Right config -> pure $ Right $ Just config
