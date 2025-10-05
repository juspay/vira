-- | Attic configuration file parsing
module Attic.Config (
  AtticConfig (..),
  AtticServerConfig (..),
  readAtticConfig,
  TOML.TOMLError,
) where

import Attic.Types (AtticServerEndpoint (..), AtticToken (..))
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
    (AtticServerConfig . AtticServerEndpoint <$> getField "endpoint")
      <*> (fmap AtticToken <$> getFields ["token"] <|> pure Nothing)

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
