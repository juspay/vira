{-# LANGUAGE OverloadedStrings #-}

{- | Nix version detection

Provides functionality to detect the installed Nix version.
-}
module System.Nix.Version (
  getVersion,
  parseVersion,
  NixVersion (..),
) where

import Effectful (Eff, IOE, (:>))
import System.Nix.Core (nix)
import System.Process.Typed (proc, readProcessStdout_)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (char, digitChar)

-- | Nix version information
newtype NixVersion = NixVersion Text
  deriving stock (Eq, Show)

{- | Get the installed Nix version

Runs `nix --version` and parses the output.
Example output: "nix (Nix) 2.18.1"
-}
getVersion :: (IOE :> es) => Eff es (Either Text NixVersion)
getVersion = do
  output <- liftIO $ readProcessStdout_ $ proc nix ["--version"]
  pure $ parseVersion $ decodeUtf8 output

-- | Parse Nix version from command output
parseVersion :: Text -> Either Text NixVersion
parseVersion txt =
  first (toText . errorBundlePretty) $ parse versionParser "" txt

type Parser = Parsec Void Text

-- | Parser for nix version output: "nix (Nix) 2.18.1"
versionParser :: Parser NixVersion
versionParser = do
  -- Skip prefix (e.g., "nix (Nix) ") up to the version number
  _ <- MP.manyTill MP.anySingle (MP.lookAhead digitChar)
  -- Parse version number (e.g., "2.18.1")
  NixVersion <$> versionNumber

-- | Parse a version number like "2.18.1"
versionNumber :: Parser Text
versionNumber = do
  major <- MP.some digitChar
  _ <- char '.'
  minor <- MP.some digitChar
  patch <- MP.optional $ do
    _ <- char '.'
    MP.some digitChar
  pure $ toText $ major <> "." <> minor <> maybe "" ("." <>) patch
