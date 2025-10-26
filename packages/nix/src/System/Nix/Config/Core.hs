{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Nix configuration and remote builders - core implementation
module System.Nix.Config.Core (
  NixConfig (..),
  NixConfigField (..),
  RemoteBuilder (..),
  nixConfigShow,
  parseBuilderValue,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import Data.Char (isUpper, toLower)
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
import System.Nix.System (System)
import Text.Megaparsec (parse)

-- | Wrapper for config fields with metadata from JSON
data NixConfigField a = NixConfigField
  { value :: a
  -- ^ Actual config value
  , description :: Text
  -- ^ Description from nix.conf (for UI tooltips)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Nix configuration
data NixConfig = NixConfig
  { builders :: [RemoteBuilder]
  -- ^ Remote builders configured in the system
  , maxJobs :: NixConfigField Natural
  -- ^ Maximum parallel build jobs (0 means auto)
  , cores :: NixConfigField Natural
  -- ^ CPU cores available per build job (0 means all cores)
  , substituters :: NixConfigField [Text]
  -- ^ Binary cache URLs (Nix caches)
  , trustedPublicKeys :: NixConfigField [Text]
  -- ^ Public keys for cache verification
  , system :: NixConfigField System
  -- ^ Current system architecture
  , extraPlatforms :: NixConfigField [System]
  -- ^ Additional platforms this system can build for
  , experimentalFeatures :: NixConfigField [Text]
  -- ^ Enabled experimental Nix features
  , rawConfig :: Aeson.Value
  -- ^ Full raw JSON for debugging/inspection
  }
  deriving stock (Show, Eq)

-- | Raw config structure matching JSON output from `nix config show --json`
data NixConfigRaw = NixConfigRaw
  { maxJobs :: NixConfigField Natural
  , cores :: NixConfigField Natural
  , substituters :: NixConfigField [Text]
  , trustedPublicKeys :: NixConfigField [Text]
  , system :: NixConfigField System
  , extraPlatforms :: NixConfigField [System]
  , experimentalFeatures :: NixConfigField [Text]
  , builders :: NixConfigField Text
  -- ^ Builders as text (needs separate parsing)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON NixConfigRaw where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelToKebab}

{- | Convert camelCase to kebab-case for JSON field names
maxJobs -> max-jobs, trustedPublicKeys -> trusted-public-keys
-}
camelToKebab :: String -> String
camelToKebab = go
  where
    go [] = []
    go (x : xs)
      | isUpper x = '-' : toLower x : go xs
      | otherwise = x : go xs

-- | Parse the output of `nix config show --json`
nixConfigShow ::
  ( Error Text :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , Process :> es
  , IOE :> es
  ) =>
  Eff es NixConfig
nixConfigShow = do
  let cmd = proc nix ["config", "show", "--json"]
  withLogCommand cmd $ do
    log Info "Reading Nix configuration (JSON)"
    output <-
      readCreateProcess cmd ""
        `catchIO` \ex -> do
          throwError $ "nix config show --json failed: " <> show @Text ex

    -- Parse JSON with aeson
    rawConfig <- case Aeson.eitherDecode (encodeUtf8 $ toText output) of
      Left err -> throwError $ "Failed to parse nix config JSON: " <> toText err
      Right cfg -> pure cfg

    NixConfigRaw {maxJobs, cores, substituters, trustedPublicKeys, system, extraPlatforms, experimentalFeatures, builders} <- case Aeson.fromJSON rawConfig of
      Aeson.Error err -> throwError $ "Failed to decode config fields: " <> toText err
      Aeson.Success v -> pure v

    -- Parse builders separately from text value
    let buildersText = builders.value
    parsedBuilders <- parseBuilderValue buildersText

    pure
      NixConfig
        { builders = parsedBuilders
        , maxJobs
        , cores
        , substituters
        , trustedPublicKeys
        , system
        , extraPlatforms
        , experimentalFeatures
        , rawConfig
        }

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
  | T.null val = pure []
  | otherwise = case parse pBuilders "<inline>" val of
      Left err -> throwError $ "Failed to parse builders: " <> show @Text err
      Right bs -> pure bs
