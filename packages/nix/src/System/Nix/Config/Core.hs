{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Nix configuration and remote builders - core implementation
module System.Nix.Config.Core (
  NixConfig (..),
  NixConfigField (..),
  Builders (..),
  RemoteBuilder (..),
  nixConfigShow,
  resolveBuilders,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import Data.Char (isUpper, toLower)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Colog.Simple.Process (withLogCommand)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catchIO)
import Effectful.Process (Process, proc, readCreateProcess)
import Effectful.Reader.Static qualified as ER
import System.Directory (doesFileExist)
import System.Nix.Config.Builders (Builders (..), RemoteBuilder (..), pBuilders)
import System.Nix.Core (nix)
import System.Nix.System (System)
import Text.Megaparsec (parse)

-- | A field in `nix.conf`.
data NixConfigField a = NixConfigField
  { value :: a
  -- ^ Actual config value
  , description :: Text
  -- ^ Description of the config field
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Nix configuration from `nix.conf`
data NixConfig = NixConfig
  { builders :: NixConfigField Builders
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
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON NixConfig where
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

-- | Read Nix configuration from `nix config show --json`
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

    -- Parse JSON directly into NixConfig
    case Aeson.eitherDecode (encodeUtf8 $ toText output) of
      Left err -> throwError $ "Failed to parse nix config JSON: " <> toText err
      Right cfg -> pure cfg

-- | Resolve builders specification into a list of remote builders
resolveBuilders :: (Error Text :> es, IOE :> es) => Builders -> Eff es [RemoteBuilder]
resolveBuilders = \case
  BuildersEmpty -> pure []
  BuildersList bs -> pure bs
  BuildersFile filePath -> do
    -- Allow missing builders file - Nix allows this and treats it as empty
    exists <- liftIO $ doesFileExist filePath
    if not exists
      then pure []
      else do
        buildersContent <- decodeUtf8 <$> readFileBS filePath
        case parse pBuilders filePath buildersContent of
          Left err -> throwError $ "Failed to parse builders file: " <> show @Text err
          Right bs -> pure bs
