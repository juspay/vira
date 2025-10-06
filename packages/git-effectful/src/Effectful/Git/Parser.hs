{-# LANGUAGE RecordWildCards #-}

{- | Git output parsers

This module contains parsers for various git command outputs.
-}
module Effectful.Git.Parser (
  gitRefParser,
) where

import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Effectful.Git.Types (BranchName (..), Commit (..))
import Text.Megaparsec (Parsec, anySingle, manyTill, takeRest)
import Text.Megaparsec.Char (tab)

-- | Parse a git ref line into a branch name and commit
gitRefParser :: Parsec Void Text (BranchName, Commit)
gitRefParser = do
  branchName' <- toText <$> manyTill anySingle tab
  cid <- fromString <$> manyTill anySingle tab
  timestampStr <- manyTill anySingle tab
  author <- toText <$> manyTill anySingle tab
  authorEmailRaw <- toText <$> manyTill anySingle tab
  message <- takeRest

  -- Strip "origin/" prefix from branch name if present to get clean branch names
  let branchName = fromString . toString $ T.stripPrefix "origin/" branchName' ?: branchName'

  -- Strip angle brackets from email if present (git %(authoremail) includes < >)
  let authorEmail = T.strip $ fromMaybe authorEmailRaw $ do
        stripped1 <- T.stripPrefix "<" authorEmailRaw
        T.stripSuffix ">" stripped1

  timestamp <- maybe (fail $ "Invalid timestamp: " <> timestampStr) return (readMaybe timestampStr)
  let date = posixSecondsToUTCTime (fromIntegral (timestamp :: Int))

  let commit = Commit {id = cid, ..}
  return (branchName, commit)
