{-# LANGUAGE RecordWildCards #-}

{- | Git for-each-ref command and parsing

This module handles the git for-each-ref command and parsing of its output.
-}
module Effectful.Git.Command.ForEachRef (
  -- * Command
  forEachRefRemoteBranches,

  -- * Parser
  gitRefParser,

  -- * Operations
  remoteBranchesFromClone,
) where

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, log)
import Effectful.Colog.Simple.Process (withLogCommand)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catchIO)
import Effectful.Git.Core (git)
import Effectful.Git.Types (BranchName (..), Commit (..))
import Effectful.Process (CreateProcess (..), Process, proc, readCreateProcess)
import Effectful.Reader.Static qualified as ER
import Text.Megaparsec (Parsec, anySingle, manyTill, parse, takeRest)
import Text.Megaparsec.Char (string, tab)

{- | Get remote branches from a git clone.

This function expects the clone to already exist and be updated.
It parses 'BranchName's and 'Commit's from the existing clone without modifying it.
-}
remoteBranchesFromClone :: (Error Text :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es, Process :> es, IOE :> es) => FilePath -> Eff es (Map BranchName Commit)
remoteBranchesFromClone clonePath = do
  output <-
    withLogCommand forEachRefRemoteBranches $ do
      log Debug "Running git for-each-ref"
      readCreateProcess forEachRefRemoteBranches {cwd = Just clonePath} ""
        `catchIO` \ex -> do
          let errorMsg = "Git for-each-ref failed: " <> show ex
          log Error errorMsg
          throwError $ toText errorMsg

  -- Parse each line; gitRefParser returns Nothing for bare remote names (filtered out by mapMaybe)
  let gitRefLines = lines $ T.strip (toText output)
  case traverse parseCommitLine gitRefLines of
    Left err -> throwError err
    Right commits -> return $ Map.fromList $ catMaybes commits
  where
    parseCommitLine :: Text -> Either Text (Maybe (BranchName, Commit))
    parseCommitLine line = case parse gitRefParser "" line of
      Left err ->
        Left $ "Parse error on line '" <> line <> "': " <> show err
      Right result -> Right result

-- | Return the 'CreateProcess' to list remote branches with metadata
forEachRefRemoteBranches :: CreateProcess
forEachRefRemoteBranches =
  proc
    git
    [ "for-each-ref"
    , "--format=%(refname:short)%09%(objectname)%09%(committerdate:unix)%09%(authorname)%09%(authoremail)%09%(subject)"
    , "refs/remotes"
    ]

{- | Parse a git ref line into a 'BranchName' and 'Commit'.

Assumes the remote is named "origin". Returns 'Nothing' for bare remote names
(like "origin") that don't have a branch component, filtering out remote HEAD
refs that aren't actual branches (issue #282).
-}
gitRefParser :: Parsec Void Text (Maybe (BranchName, Commit))
gitRefParser = do
  -- Try to parse "origin/" prefix; if missing (bare "origin"), return Nothing
  mOriginPrefix <- optional (string "origin/")
  case mOriginPrefix of
    Nothing -> do
      -- Consume rest of line and return Nothing (bare remote name)
      takeRest $> Nothing
    Just _ -> do
      -- Parse branch name (everything until tab)
      branchNameStr <- toText <$> manyTill anySingle tab
      cid <- fromString <$> manyTill anySingle tab
      timestampStr <- manyTill anySingle tab
      author <- toText <$> manyTill anySingle tab
      authorEmailRaw <- toText <$> manyTill anySingle tab
      message <- takeRest

      let branchName = fromString $ toString branchNameStr

      -- Strip angle brackets from email if present (git %(authoremail) includes < >)
      let authorEmail = T.strip $ fromMaybe authorEmailRaw $ do
            stripped1 <- T.stripPrefix "<" authorEmailRaw
            T.stripSuffix ">" stripped1

      timestamp <- maybe (fail $ "Invalid timestamp: " <> timestampStr) return (readMaybe timestampStr)
      let date = posixSecondsToUTCTime (fromIntegral (timestamp :: Int))

      let commit = Commit {id = cid, ..}
      return $ Just (branchName, commit)
