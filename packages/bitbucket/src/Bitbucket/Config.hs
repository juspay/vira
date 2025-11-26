{- | Bitbucket configuration management

Reads configuration from ~/.config/bb/config file.
-}
module Bitbucket.Config (
  loadConfig,
  ConfigError (..),
) where

import Bitbucket.API.V1.Core (BitbucketConfig (..))
import Bitbucket.ConfigPath (getConfigPath)
import Data.Text qualified as T
import Network.HTTP.Req (useHttpsURI)
import System.Directory (doesFileExist)
import Text.URI (mkURI)

-- | Configuration loading errors
data ConfigError
  = ConfigFileNotFound FilePath
  | ConfigParseError Text
  | InvalidUrl Text
  deriving stock (Show, Eq)

{- | Load Bitbucket configuration from config file

Config file format (simple key=value):
@
baseUrl=https://bitbucket.juspay.net
token=BBDC-xyz...
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
      let content = decodeUtf8 contentBS
      case parseConfig content of
        Left err -> pure $ Left err
        Right cfg -> pure $ Right cfg

-- | Parse config file content
parseConfig :: Text -> Either ConfigError BitbucketConfig
parseConfig content = do
  let pairs = parsePairs content
  baseUrlText <- lookupKey "baseUrl" pairs
  token <- lookupKey "token" pairs

  -- Parse URL using text-uri and req
  uri <- case mkURI baseUrlText of
    Left _ -> Left $ InvalidUrl baseUrlText
    Right u -> Right u

  baseUrl <- case useHttpsURI uri of
    Just (url, _) -> Right url
    Nothing -> Left $ InvalidUrl baseUrlText

  pure $ BitbucketConfig {baseUrl, token}

-- | Parse key=value pairs from config file
parsePairs :: Text -> [(Text, Text)]
parsePairs content =
  mapMaybe parseLine $ lines content
  where
    parseLine line =
      let trimmed = T.strip line
       in if T.null trimmed || "#" `T.isPrefixOf` trimmed
            then Nothing
            else case T.splitOn "=" trimmed of
              [key, value] -> Just (T.strip key, T.strip value)
              _ -> Nothing

-- | Lookup a key in parsed config
lookupKey :: Text -> [(Text, Text)] -> Either ConfigError Text
lookupKey key pairs =
  case find (\(k, _) -> k == key) pairs of
    Nothing -> Left $ ConfigParseError $ "Missing required key: " <> key
    Just (_, value) -> Right value
