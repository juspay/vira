{-# LANGUAGE TemplateHaskell #-}

{- | Module for working with Git repositories in Haskell

At one point, we should release this as a separate package to Hackage. Beam & Servant instances should be gated behind a Cabal flag.
-}
module Vira.Lib.Git where

import Data.Aeson
import Data.ByteString qualified as B
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Data.SafeCopy
import Data.Text qualified as T
import Database.Beam (FromBackendRow, HasSqlEqualityCheck)
import Database.Beam.Backend (HasSqlValueSyntax)
import Database.Beam.Sqlite.Connection (Sqlite)
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
import Servant (FromHttpApiData, ToHttpApiData)
import System.Process
import System.Which (staticWhich)
import Test.Hspec

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
    , FromBackendRow Sqlite
    , HasSqlValueSyntax SqliteValueSyntax
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
    , FromBackendRow Sqlite
    , HasSqlValueSyntax SqliteValueSyntax
    , HasSqlEqualityCheck Sqlite
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

spec :: Spec
spec = do
  describe "Git" $ do
    it "remoteBranches" $ do
      let archivedRepo = "https://github.com/srid/leptos-nix-template" -- Archived; won't change
      branches <- remoteBranches archivedRepo
      Map.lookup "main" branches `shouldBe` Just "68506f5bf0a5883e737c0f8b7bab4c651a0d5fc0"
