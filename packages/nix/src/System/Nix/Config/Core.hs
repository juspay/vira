{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Nix configuration and remote builders - core implementation
module System.Nix.Config.Core (
  NixConfig (..),
  RemoteBuilder (..),
  nixConfigShow,
  parseConfigOutput,
  parseBuilderValue,
  pConfigFile,
  pConfigLine,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Colog.Simple.Process (withLogCommand)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catchIO)
import Effectful.Process (Process, proc, readCreateProcess)
import Effectful.Reader.Static qualified as ER
import System.Nix.Config.Machine (RemoteBuilder (..), pBuilders)
import System.Nix.Core (nix)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many)

-- | Nix configuration
data NixConfig = NixConfig
  { builders :: [RemoteBuilder]
  -- ^ Remote builders configured in the system
  , rawConfig :: Map Text Text
  -- ^ Raw configuration key-value pairs
  }
  deriving stock (Show, Eq)

-- | Parse the output of `nix config show`
nixConfigShow ::
  ( Error Text :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  Eff es NixConfig
nixConfigShow = do
  let cmd = proc nix ["config", "show"]
  withLogCommand cmd $ do
    log Info "Reading Nix configuration"
    output <-
      readCreateProcess cmd ""
        `catchIO` \ex -> do
          throwError $ "nix config show failed: " <> show @Text ex

    rawConfig <- case parseConfigOutput (encodeUtf8 $ toText output) of
      Left err -> throwError err
      Right cfg -> pure cfg

    builders <- case Map.lookup "builders" rawConfig of
      Nothing -> pure []
      Just val -> parseBuilderValue val

    pure NixConfig {builders, rawConfig}

-- | Parse builder value (either @file or inline)
parseBuilderValue :: (Error Text :> es, IOE :> es) => Text -> Eff es [RemoteBuilder]
parseBuilderValue val
  | T.isPrefixOf "@" val = do
      let filePath = toString $ T.drop 1 val
      buildersContent <- decodeUtf8 <$> readFileBS filePath
      case parse pBuilders filePath buildersContent of
        Left err -> throwError $ "Failed to parse builders file: " <> show @Text err
        Right bs -> pure bs
  | otherwise = case parse pBuilders "<inline>" val of
      Left err -> throwError $ "Failed to parse builders: " <> show @Text err
      Right bs -> pure bs

-- | Parse the key-value output from `nix config show`
parseConfigOutput :: LByteString -> Either Text (Map Text Text)
parseConfigOutput output =
  case parse pConfigFile "<nix config show>" (decodeUtf8 output) of
    Left err -> Left $ "Failed to parse nix config show output: " <> show err
    Right cfg -> Right cfg

-- | Parser for nix config show output
pConfigFile :: Parsec Void Text (Map Text Text)
pConfigFile = do
  pairs <- many (try pConfigLine)
  space
  eof
  pure $ Map.fromList pairs

-- | Parser for a single config line: "key = value"
pConfigLine :: Parsec Void Text (Text, Text)
pConfigLine = do
  key <- toText <$> someTill anySingle (string " = ")
  value <- toText <$> manyTill anySingle eol
  pure (key, value)
