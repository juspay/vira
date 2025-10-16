{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Nix configuration and remote builders
module System.Nix.Config (
  NixConfig (..),
  RemoteBuilder (..),
  nixConfigShow,
  pConfigFile,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Nix.Config.Machine (RemoteBuilder (..), pBuilders)
import System.Nix.Core (nix)
import System.Process (readProcess)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many)

type Parser = Parsec Void Text

-- | Nix configuration
data NixConfig = NixConfig
  { builders :: [RemoteBuilder]
  -- ^ Remote builders configured in the system
  , rawConfig :: Map Text Text
  -- ^ Raw configuration key-value pairs
  }
  deriving stock (Show, Eq)

-- | Parse the output of `nix config show`
nixConfigShow :: (MonadIO m) => m (Either Text NixConfig)
nixConfigShow = do
  output <- encodeUtf8 . toText <$> liftIO (readProcess nix ["config", "show"] "")
  case parseConfigOutput output of
    Left err -> pure $ Left err
    Right rawConfig -> do
      builders <- case Map.lookup "builders" rawConfig of
        Nothing -> pure $ Right []
        Just val
          | T.isPrefixOf "@" val -> do
              let filePath = toString $ T.drop 1 val
              buildersContent <- decodeUtf8 <$> readFileBS filePath
              case parse pBuilders filePath buildersContent of
                Left err -> pure $ Left $ "Failed to parse builders file: " <> show err
                Right bs -> pure $ Right bs
          | otherwise -> case parse pBuilders "<inline>" val of
              Left err -> pure $ Left $ "Failed to parse builders: " <> show err
              Right bs -> pure $ Right bs
      case builders of
        Left err -> pure $ Left err
        Right bs -> pure $ Right NixConfig {builders = bs, rawConfig}

-- | Parse the key-value output from `nix config show`
parseConfigOutput :: LByteString -> Either Text (Map Text Text)
parseConfigOutput output =
  case parse pConfigFile "<nix config show>" (decodeUtf8 output) of
    Left err -> Left $ "Failed to parse nix config show output: " <> show err
    Right cfg -> Right cfg

-- | Parser for nix config show output
pConfigFile :: Parser (Map Text Text)
pConfigFile = do
  pairs <- many (try pConfigLine)
  space
  eof
  pure $ Map.fromList pairs

-- | Parser for a single config line: "key = value"
pConfigLine :: Parser (Text, Text)
pConfigLine = do
  key <- toText <$> someTill anySingle (string " = ")
  value <- toText <$> manyTill anySingle eol
  pure (key, value)
