{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Nix remote builder (machine) specification
module System.Nix.Config.Machine (
  RemoteBuilder (..),
  pBuilders,
) where

import Data.Text qualified as T
import System.Nix.System (System (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (many)

-- | Remote builder specification from Nix machines file
data RemoteBuilder = RemoteBuilder
  { uri :: Text
  -- ^ SSH URI (e.g., "ssh-ng://user@host")
  , platforms :: [System]
  -- ^ Supported platforms (e.g., ["aarch64-darwin"])
  , sshKey :: Maybe FilePath
  -- ^ Path to SSH key
  , maxJobs :: Int
  -- ^ Maximum number of parallel jobs
  , speedFactor :: Int
  -- ^ Speed factor for prioritization
  , supportedFeatures :: [Text]
  -- ^ Supported features (e.g., ["benchmark", "big-parallel"])
  , mandatoryFeatures :: [Text]
  -- ^ Mandatory features
  , publicHostKey :: Maybe Text
  -- ^ Base64-encoded public host key
  }
  deriving stock (Show, Eq)

type Parser = Parsec Void Text

-- | Parser for builders/machines file
pBuilders :: Parser [RemoteBuilder]
pBuilders = catMaybes <$> many pBuilderLine <* eof

{- | Parser for a single builder line
Format: URI PLATFORMS KEY MAX_JOBS SPEED_FACTOR FEATURES MANDATORY_FEATURES PUBLIC_HOST_KEY
-}
pBuilderLine :: Parser (Maybe RemoteBuilder)
pBuilderLine = do
  -- Skip empty lines and comments
  (Nothing <$ (space1 <|> pComment))
    <|> (Just <$> pBuilder)

pComment :: Parser ()
pComment = char '#' *> manyTill anySingle eol $> ()

pBuilder :: Parser RemoteBuilder
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
pField :: Parser Text
pField = toText <$> (space *> someTill anySingle space1)

-- | Parse the last field on a line (followed by newline, not whitespace)
pLastField :: Parser Text
pLastField = toText <$> (space *> someTill anySingle (lookAhead eol))

-- | Parse an optional last field (treats "-" as Nothing)
pOptionalLastField :: Parser (Maybe Text)
pOptionalLastField = do
  field <- optional pLastField
  pure $ case field of
    Just "-" -> Nothing
    x -> x

-- | Parse an optional field (treats "-" as Nothing)
pOptionalField :: Parser (Maybe Text)
pOptionalField = do
  field <- pField
  pure $ if field == "-" then Nothing else Just field

-- | Parse an integer field
pIntField :: Parser Int
pIntField = space *> L.decimal <* space1
