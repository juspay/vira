-- | Attic configuration file parsing
module Attic.Config (
  AtticConfig (..),
  AtticServerConfig (..),
  readAtticConfig,
) where

import System.Directory (XdgDirectory (..), doesFileExist, getXdgDirectory)
import TOML (DecodeTOML, getField, getFields)
import TOML qualified

-- | Attic server configuration from config.toml
data AtticServerConfig = AtticServerConfig
  { endpoint :: Text
  , token :: Maybe Text
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

{- | Read Attic configuration from ~/.config/attic/config.toml

Returns Nothing if the config file doesn't exist (not configured).
-}
readAtticConfig :: IO (Maybe AtticConfig)
readAtticConfig = do
  configPath <- getXdgDirectory XdgConfig "attic/config.toml"
  exists <- doesFileExist configPath
  if not exists
    then pure Nothing
    else do
      contents <- readFileBS configPath <&> decodeUtf8
      case TOML.decode contents of
        Left err -> do
          putTextLn $ "Warning: Failed to parse Attic config: " <> show err
          pure Nothing
        Right config -> pure $ Just config
