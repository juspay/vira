{-# LANGUAGE TemplateHaskell #-}

{- | Module for working with Git repositories in Haskell

At one point, we should release this as a separate package to Hackage. Servant instances should be gated behind a Cabal flag.
-}
module Vira.Lib.Git where

import Data.Aeson
import Data.ByteString qualified as B
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Data.SafeCopy
import Data.Text qualified as T
import Servant (FromHttpApiData, ToHttpApiData)
import System.Process
import System.Which (staticWhich)

{- | Path to the `git` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
git :: FilePath
git = $(staticWhich "git")

-- | Git commit hash
newtype CommitID = CommitID {unCommitID :: Text}
  deriving stock (Generic, Show, Eq, Ord, Data)
  deriving newtype
    ( IsString
    , ToJSON
    , ToString
    , ToHttpApiData
    , FromHttpApiData
    )

-- | Git branch name
newtype BranchName = BranchName {unBranchName :: Text}
  deriving stock (Generic, Data)
  deriving newtype (Show, Eq, Ord)
  deriving newtype
    ( IsString
    , ToJSON
    , ToString
    , ToHttpApiData
    , FromHttpApiData
    )

$(deriveSafeCopy 0 'base ''CommitID)
$(deriveSafeCopy 0 'base ''BranchName)

{- | Get all branches available in the remote.

Run `git ls-remote` and filter it to only include branches.

https://git-scm.com/docs/git-ls-remote
-}
remoteBranches :: Text -> IO (Map BranchName CommitID)
remoteBranches url = do
  -- Use System.Process to run and parse output
  output <- readProcess git ["ls-remote", "--exit-code", "--branches", toString url, refsPattern] ""
  return $ Map.fromList $ mapMaybe parseLine $ lines $ toText output
  where
    knownPrefix :: ByteString = "refs/heads/"
    refsPattern = decodeUtf8 $ knownPrefix <> "*"
    parseLine :: Text -> Maybe (BranchName, CommitID)
    parseLine line = case T.splitOn "\t" line of
      [hash, name'] ->
        let name = T.drop (B.length knownPrefix) name'
         in Just (fromString . toString $ name, fromString . toString $ hash)
      _unexpectedPartitions -> Nothing

-- | Return the `CreateProcess` to clone a repo at a specific commit
cloneAtCommit :: Text -> CommitID -> CreateProcess
cloneAtCommit url commit =
  proc
    git
    [ "-c"
    , "advice.detachedHead=false"
    , "clone"
    , "--single-branch"
    , "--depth"
    , "1"
    , "--revision"
    , toString commit
    , toString url
    , "."
    ]
