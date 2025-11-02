{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Nix builders configuration and remote builder specification
module System.Nix.Config.Builders (
  Builders (..),
  RemoteBuilder (..),
  pBuilders,
  resolveBuilders,
) where

import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import System.Directory (doesFileExist)
import System.Nix.System (System (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (many)

-- | Builders specification - either inline or file reference
data Builders
  = -- | Inline builder specifications
    BuildersList [RemoteBuilder]
  | -- | File path reference (e.g., @\@/path/to/machines@)
    BuildersFile FilePath
  | -- | No builders configured
    BuildersEmpty
  deriving stock (Show, Eq)

instance FromJSON Builders where
  parseJSON = Aeson.withText "Builders" $ \txt ->
    if T.null txt
      then pure BuildersEmpty
      else
        if T.isPrefixOf "@" txt
          then pure $ BuildersFile (toString $ T.drop 1 txt)
          else case parse pBuilders "<inline>" txt of
            Left err -> fail $ "Failed to parse inline builders: " <> show err
            Right bs -> pure $ BuildersList bs

-- | Remote builder specification from Nix machines file
data RemoteBuilder = RemoteBuilder
  { uri :: Text
  -- ^ SSH URI (e.g., @ssh-ng:\/\/user\@host@)
  , platforms :: [System]
  -- ^ Supported 'System.Nix.System.System's (e.g., @[aarch64-darwin]@)
  , sshKey :: Maybe FilePath
  -- ^ Path to SSH key
  , maxJobs :: Int
  -- ^ Maximum number of parallel jobs
  , speedFactor :: Int
  -- ^ Speed factor for prioritization
  , supportedFeatures :: [Text]
  -- ^ Supported features (e.g., @[benchmark, big-parallel]@)
  , mandatoryFeatures :: [Text]
  -- ^ Mandatory features
  , publicHostKey :: Maybe Text
  -- ^ Base64-encoded public host key
  }
  deriving stock (Show, Eq)

-- | Parser for builders/machines file
pBuilders :: Parsec Void Text [RemoteBuilder]
pBuilders = catMaybes <$> many pBuilderLine <* eof

{- | Parser for a single builder line
Format: URI PLATFORMS KEY MAX_JOBS SPEED_FACTOR FEATURES MANDATORY_FEATURES PUBLIC_HOST_KEY
-}
pBuilderLine :: Parsec Void Text (Maybe RemoteBuilder)
pBuilderLine = do
  -- Skip empty lines and comments
  (Nothing <$ (space1 <|> pComment))
    <|> (Just <$> pBuilder)

pComment :: Parsec Void Text ()
pComment = char '#' *> manyTill anySingle eol $> ()

pBuilder :: Parsec Void Text RemoteBuilder
pBuilder = do
  uri <- pField
  platforms <- parsePlatforms <$> pField
  sshKey <- pOptionalField
  maxJobs <- pIntField
  speedFactor <- pIntField
  supportedFeatures <- parseTextList <$> pField
  mandatoryFeatures <- parseTextList <$> pField
  publicHostKey <- pOptionalLastField
  _ <- eol
  pure
    RemoteBuilder
      { uri
      , platforms
      , sshKey = toString <$> sshKey
      , maxJobs
      , speedFactor
      , supportedFeatures
      , mandatoryFeatures
      , publicHostKey
      }
  where
    parsePlatforms = map System . parseTextList
    parseTextList txt
      | txt == "-" = []
      | otherwise = T.splitOn "," txt

-- | Parse a whitespace-separated field
pField :: Parsec Void Text Text
pField = toText <$> (space *> someTill anySingle space1)

-- | Parse the last field on a line (followed by newline, not whitespace)
pLastField :: Parsec Void Text Text
pLastField = toText <$> (space *> someTill anySingle (lookAhead eol))

-- | Parse an optional last field (treats "-" as Nothing)
pOptionalLastField :: Parsec Void Text (Maybe Text)
pOptionalLastField = do
  field <- optional pLastField
  pure $ case field of
    Just "-" -> Nothing
    x -> x

-- | Parse an optional field (treats "-" as Nothing)
pOptionalField :: Parsec Void Text (Maybe Text)
pOptionalField = do
  field <- pField
  pure $ if field == "-" then Nothing else Just field

-- | Parse an integer field
pIntField :: Parsec Void Text Int
pIntField = space *> L.decimal <* space1

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
