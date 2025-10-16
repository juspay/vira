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
import Effectful.Colog.Simple.Process (logCommand)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catchIO)
import Effectful.Git.Core (git)
import Effectful.Git.Types (BranchName (..), Commit (..))
import Effectful.Process (CreateProcess (..), Process, proc, readCreateProcess)
import Effectful.Reader.Static qualified as ER
import Text.Megaparsec (Parsec, anySingle, manyTill, parse, takeRest)
import Text.Megaparsec.Char (tab)

{- | Get remote branches from a git clone.
This function expects the clone to already exist and be updated.
It parses branches from the existing clone without modifying it.
-}
remoteBranchesFromClone :: (Error Text :> es, Log (RichMessage IO) :> es, ER.Reader LogContext :> es, Process :> es, IOE :> es) => FilePath -> Eff es (Map BranchName Commit)
remoteBranchesFromClone clonePath = do
  logCommand Debug "Running git for-each-ref in clone" forEachRefRemoteBranches

  output <-
    readCreateProcess forEachRefRemoteBranches {cwd = Just clonePath} ""
      `catchIO` \ex -> do
        let errorMsg = "Git for-each-ref failed: " <> show ex
        log Error errorMsg
        throwError $ toText errorMsg

  -- Drop the first line, which is 'origin' (not a branch)
  let gitRefLines = drop 1 $ lines $ T.strip (toText output)
  case traverse parseCommitLine gitRefLines of
    Left err -> throwError err
    Right commits -> return $ Map.fromList commits
  where
    parseCommitLine :: Text -> Either Text (BranchName, Commit)
    parseCommitLine line = case parse gitRefParser "" line of
      Left err ->
        Left $ "Parse error on line '" <> line <> "': " <> show err
      Right result -> Right result

-- | Return the `CreateProcess` to list remote branches with metadata
forEachRefRemoteBranches :: CreateProcess
forEachRefRemoteBranches =
  proc
    git
    [ "for-each-ref"
    , "--format=%(refname:short)%09%(objectname)%09%(committerdate:unix)%09%(authorname)%09%(authoremail)%09%(subject)"
    , "refs/remotes"
    ]

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
