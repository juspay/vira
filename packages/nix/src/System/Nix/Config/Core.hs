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
  parseNatural,
  parseSpaceSeparated,
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
import System.Directory (doesFileExist)
import System.Nix.Config.Machine (RemoteBuilder (..), pBuilders)
import System.Nix.Core (nix)
import System.Nix.System (System (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many)

-- | Nix configuration
data NixConfig = NixConfig
  { builders :: [RemoteBuilder]
  -- ^ Remote builders configured in the system
  , maxJobs :: Natural
  -- ^ Maximum parallel build jobs (0 means auto)
  , cores :: Natural
  -- ^ CPU cores available per build job (0 means all cores)
  , substituters :: [Text]
  -- ^ Binary cache URLs (Nix caches)
  , trustedPublicKeys :: [Text]
  -- ^ Public keys for cache verification
  , system :: System
  -- ^ Current system architecture
  , extraPlatforms :: [System]
  -- ^ Additional platforms this system can build for
  , experimentalFeatures :: [Text]
  -- ^ Enabled experimental Nix features
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

    -- Parse required fields, surface errors
    maxJobs <- parseNatural "max-jobs" rawConfig
    cores <- parseNatural "cores" rawConfig
    system <- case Map.lookup "system" rawConfig of
      Nothing -> throwError @Text "Missing required field: system"
      Just s | T.null s -> throwError @Text "Empty system field"
      Just s -> pure $ fromString $ toString s

    -- Parse list fields (allow missing/empty)
    let substituters = parseSpaceSeparated "substituters" rawConfig
        trustedPublicKeys = parseSpaceSeparated "trusted-public-keys" rawConfig
        extraPlatforms = map (fromString . toString) $ parseSpaceSeparated "extra-platforms" rawConfig
        experimentalFeatures = parseSpaceSeparated "experimental-features" rawConfig

    -- Parse builders
    builders <- case Map.lookup "builders" rawConfig of
      Nothing -> pure []
      Just val -> parseBuilderValue val

    pure NixConfig {builders, maxJobs, cores, substituters, trustedPublicKeys, system, extraPlatforms, experimentalFeatures, rawConfig}

-- | Parse builder value (either @file or inline)
parseBuilderValue :: (Error Text :> es, IOE :> es) => Text -> Eff es [RemoteBuilder]
parseBuilderValue val
  | T.isPrefixOf "@" val = do
      let filePath = toString $ T.drop 1 val
      -- Allow missing builders file - Nix allows this and treats it as empty
      exists <- liftIO $ doesFileExist filePath
      if not exists
        then pure []
        else do
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

-- | Parse Natural from config, surface errors
parseNatural :: (Error Text :> es) => Text -> Map Text Text -> Eff es Natural
parseNatural field cfg = case Map.lookup field cfg of
  Nothing -> throwError $ "Missing field: " <> field
  Just val -> case readMaybe @Natural (toString val) of
    Nothing -> throwError $ "Invalid number for " <> field <> ": " <> val
    Just n -> pure n

-- | Parse space-separated list, return [] if missing/empty
parseSpaceSeparated :: Text -> Map Text Text -> [Text]
parseSpaceSeparated field cfg =
  maybe [] (filter (not . T.null) . words) (Map.lookup field cfg)
