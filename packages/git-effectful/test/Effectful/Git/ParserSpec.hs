{-# LANGUAGE OverloadedRecordDot #-}

module Effectful.Git.ParserSpec where

import Effectful.Git.Parser (gitRefParser)
import Effectful.Git.Types (BranchName (..), Commit (..), CommitID (..))
import Test.Hspec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "gitRefParser" $ do
    it "parses a valid git ref line" $ do
      let input = "origin/main\tabc123def456\t1640000000\tJohn Doe\t<john@example.com>\tInitial commit"
      case parse gitRefParser "" input of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right (branchName, commit) -> do
          branchName `shouldBe` BranchName "main"
          commit.commitId `shouldBe` CommitID "abc123def456"
          commit.commitAuthor `shouldBe` "John Doe"
          commit.commitAuthorEmail `shouldBe` "john@example.com"
          commit.commitMessage `shouldBe` "Initial commit"
