{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic configuration file parsing
module Attic.Config (
  AtticConfig (..),
  AtticServerConfig (..),
  ConfigError (..),
  getAtticConfig,
  readAtticConfig,
  lookupEndpointWithToken,
  TOML.TOMLError,
) where

import Attic.Types (AtticServer (AtticServer), AtticServerEndpoint (..), AtticToken (..))
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
  | -- | No server configured for endpoint
    NoServerForEndpoint AtticServerEndpoint
  | -- | Server configured but no authentication token
    NoToken AtticServer
  deriving stock (Show, Eq)

{- | Get validated Attic configuration

Combines reading and validation into a single operation.
Returns:
- Left ConfigError if config is invalid or incomplete
- Right AtticConfig if successfully parsed and validated (empty config if file doesn't exist)
-}
getAtticConfig :: IO (Either ConfigError AtticConfig)
getAtticConfig = validateConfig <$> readAtticConfig
  where
    validateConfig :: Either TOMLError (Maybe AtticConfig) -> Either ConfigError AtticConfig
    validateConfig result = do
      mConfig <- first ParseError result
      let config = fromMaybe emptyConfig mConfig

      -- Check if any server is missing a token
      case find (\(_server, serverCfg) -> isNothing serverCfg.token) (Map.toList config.servers) of
        Just (serverName, cfg) -> Left $ NoToken $ AtticServer serverName cfg.endpoint
        Nothing -> Right config

    emptyConfig = AtticConfig {defaultServer = Nothing, servers = Map.empty}

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

{- | Get server name from endpoint in config, only if it has a token

Searches the config for a server with matching endpoint and returns the server name
only if that server has an authentication token configured.
-}
lookupEndpointWithToken ::
  AtticConfig ->
  AtticServerEndpoint ->
  Maybe Text
lookupEndpointWithToken config serverEndpoint = do
  (name, cfg) <- find matchesEndpoint (Map.toList config.servers)
  case cfg.token of
    Just _token -> Just name
    Nothing -> Nothing
  where
    matchesEndpoint (_name, serverCfg) = serverCfg.endpoint == serverEndpoint
