{-# LANGUAGE OverloadedRecordDot #-}

module Effectful.Git.ParserSpec where

import Effectful.Git.Command.ForEachRef (filterRemoteRefs, gitRefParser)
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
          commit.id `shouldBe` CommitID "abc123def456"
          commit.author `shouldBe` "John Doe"
          commit.authorEmail `shouldBe` "john@example.com"
          commit.message `shouldBe` "Initial commit"

  -- Test for issue #282: origin should be filtered out as it's not a branch
  describe "filterRemoteRefs" $ do
    it "filters out bare remote names like 'origin'" $ do
      let refs =
            [ "origin"
            , "origin/main\tabc123\t1640000000\tAuthor\t<a@b.com>\tmsg"
            , "origin/feature\tdef456\t1640000001\tAuthor\t<a@b.com>\tmsg2"
            ]
      -- origin should be excluded, but origin/main and origin/feature should remain
      filterRemoteRefs refs
        `shouldBe` [ "origin/main\tabc123\t1640000000\tAuthor\t<a@b.com>\tmsg"
                   , "origin/feature\tdef456\t1640000001\tAuthor\t<a@b.com>\tmsg2"
                   ]

    it "handles case where origin is not the first line" $ do
      let refs =
            [ "origin/main\tabc123\t1640000000\tAuthor\t<a@b.com>\tmsg"
            , "origin"
            , "origin/feature\tdef456\t1640000001\tAuthor\t<a@b.com>\tmsg2"
            ]
      -- origin should be excluded regardless of position
      filterRemoteRefs refs
        `shouldBe` [ "origin/main\tabc123\t1640000000\tAuthor\t<a@b.com>\tmsg"
                   , "origin/feature\tdef456\t1640000001\tAuthor\t<a@b.com>\tmsg2"
                   ]

    it "handles output with no origin entry at all" $ do
      let refs =
            [ "origin/main\tabc123\t1640000000\tAuthor\t<a@b.com>\tmsg"
            , "origin/feature\tdef456\t1640000001\tAuthor\t<a@b.com>\tmsg2"
            ]
      -- All branches should be kept
      filterRemoteRefs refs `shouldBe` refs
