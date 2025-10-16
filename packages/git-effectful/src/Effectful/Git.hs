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

import Colog (Severity (..))
import Colog.Message (RichMessage)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catchIO)
import Effectful.Git.Core (git)
import Effectful.Git.Logging (log)
import Effectful.Git.Parser (gitRefParser)
import Effectful.Git.Types
import Effectful.Process (CreateProcess (..), Process, proc, readCreateProcess)
import Text.Megaparsec (parse)

{- | Get remote branches from a git clone.
This function expects the clone to already exist and be updated.
It parses branches from the existing clone without modifying it.
-}
remoteBranchesFromClone :: (Error Text :> es, Log (RichMessage IO) :> es, Process :> es, IOE :> es) => FilePath -> Eff es (Map BranchName Commit)
remoteBranchesFromClone clonePath = do
  log Debug $ "Running git for-each-ref in clone: " <> show (cmdspec forEachRefRemoteBranches)

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
