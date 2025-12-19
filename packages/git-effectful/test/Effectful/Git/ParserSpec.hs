{-# LANGUAGE OverloadedRecordDot #-}

module Effectful.Git.ParserSpec where

import Effectful.Git.Command.ForEachRef (gitRefParser)
import Effectful.Git.Types (BranchName (..), Commit (..), CommitID (..))
import Test.Hspec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "gitRefParser" $ do
    it "parses a valid git ref line with origin/ prefix" $ do
      let input = "origin/main\tabc123def456\t1640000000\tJohn Doe\t<john@example.com>\tInitial commit"
      case parse gitRefParser "" input of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right Nothing -> expectationFailure "Expected Just, got Nothing"
        Right (Just (branchName, commit)) -> do
          branchName `shouldBe` BranchName "main"
          commit.id `shouldBe` CommitID "abc123def456"
          commit.author `shouldBe` "John Doe"
          commit.authorEmail `shouldBe` "john@example.com"
          commit.message `shouldBe` "Initial commit"

    -- Test for issue #282: bare remote names should return Nothing
    it "returns Nothing for bare remote name 'origin'" $ do
      let input = "origin\tabc123\t1640000000\tAuthor\t<a@b.com>\tmsg"
      case parse gitRefParser "" input of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right result -> result `shouldBe` Nothing

    it "parses refs with non-origin remote names" $ do
      let input = "upstream/feature\tdef456\t1640000001\tAuthor\t<a@b.com>\tfeature work"
      case parse gitRefParser "" input of
        Left err -> expectationFailure $ "Parse failed: " <> show err
        Right Nothing -> expectationFailure "Expected Just, got Nothing"
        Right (Just (branchName, commit)) -> do
          -- Non-origin remotes keep the full name since only "origin/" is stripped
          branchName `shouldBe` BranchName "upstream/feature"
          commit.id `shouldBe` CommitID "def456"
