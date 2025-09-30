{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Module for working with Git repositories in Haskell

A standalone library for git operations using the effectful library.
Servant instances should be gated behind a Cabal flag.
-}
module Effectful.Git (
  -- * Git executable
  git,

  -- * Types
  module Effectful.Git.Types,

  -- * Operations
  remoteBranchesFromClone,
  cloneAtCommit,
) where

import Colog (Message, Severity (..))
import Control.Exception (try)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Git.Logging (log)
import Effectful.Git.Parser (gitRefParser)
import Effectful.Git.Types
import System.Process
import System.Which (staticWhich)
import Text.Megaparsec (parse)

{- | Path to the `git` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
git :: FilePath
git = $(staticWhich "git")

{- | Get remote branches from a git clone.
This function expects the clone to already exist and be updated.
It parses branches from the existing clone without modifying it.
-}
remoteBranchesFromClone :: (Log Message :> es, IOE :> es) => FilePath -> Eff es (Either Text (Map BranchName Commit))
remoteBranchesFromClone clonePath = do
  log Info $ "Running git for-each-ref in clone: " <> show (cmdspec forEachRefRemoteBranches)

  result <-
    liftIO $
      try $
        readCreateProcess
          forEachRefRemoteBranches {cwd = Just clonePath}
          ""

  case result of
    Left (ex :: SomeException) -> do
      let errorMsg = "Git for-each-ref failed: " <> show ex
      log Error errorMsg
      return $ Left $ toText errorMsg
    Right output -> do
      -- Drop the first line, which is 'origin' (not a branch)
      let gitRefLines = drop 1 $ lines $ T.strip (toText output)
      commits <- catMaybes <$> mapM parseCommitLine gitRefLines
      return $ Right $ Map.fromList commits
  where
    parseCommitLine :: (Log Message :> es) => Text -> Eff es (Maybe (BranchName, Commit))
    parseCommitLine line = case parse gitRefParser "" line of
      Left err -> do
        log Error $ "Parse error on line '" <> line <> "': " <> toText @String (show err)
        return Nothing
      Right result -> return $ Just result

-- | Return the `CreateProcess` to clone a repo at a specific commit
cloneAtCommit :: Text -> CommitID -> FilePath -> CreateProcess
cloneAtCommit url commit path =
  proc
    git
    [ "-c"
    , "advice.detachedHead=false"
    , "clone"
    , "--depth"
    , "1"
    , "--single-branch"
    , "--revision"
    , toString commit
    , toString url
    , path
    ]

-- | Return the `CreateProcess` to list remote branches with metadata
forEachRefRemoteBranches :: CreateProcess
forEachRefRemoteBranches =
  proc
    git
    [ "for-each-ref"
    , "--format=%(refname:short)%09%(objectname)%09%(committerdate:unix)%09%(authorname)%09%(authoremail)%09%(subject)"
    , "refs/remotes"
    ]
