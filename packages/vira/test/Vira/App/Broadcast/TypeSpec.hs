{-# LANGUAGE OverloadedStrings #-}

module Vira.App.Broadcast.TypeSpec (spec) where

import Effectful.Git (RepoName (..))
import Test.Hspec
import Vira.App.Broadcast.Type (BroadcastScope (..), matchesAnyScope)
import Vira.State.Type (JobId (..))

spec :: Spec
spec = describe "Vira.App.Broadcast.Type" $ do
  describe "matchesAnyScope" $ do
    describe "RepoScope pattern matching" $ do
      it "exact RepoScope pattern matches same broadcast" $ do
        let broadcast = RepoScope (RepoName "my-repo")
            pat = RepoScope (RepoName "my-repo")
        matchesAnyScope [pat] broadcast `shouldBe` True

      it "different RepoScope pattern does not match" $ do
        let broadcast = RepoScope (RepoName "my-repo")
            pat = RepoScope (RepoName "other-repo")
        matchesAnyScope [pat] broadcast `shouldBe` False

    describe "JobScope pattern matching" $ do
      it "exact JobScope pattern matches same broadcast" $ do
        let broadcast = JobScope (Just (JobId 123))
            pat = JobScope (Just (JobId 123))
        matchesAnyScope [pat] broadcast `shouldBe` True

      it "wildcard JobScope Nothing matches any job broadcast" $ do
        let broadcast = JobScope (Just (JobId 123))
            pat = JobScope Nothing
        matchesAnyScope [pat] broadcast `shouldBe` True

      it "different JobScope pattern does not match" $ do
        let broadcast = JobScope (Just (JobId 123))
            pat = JobScope (Just (JobId 456))
        matchesAnyScope [pat] broadcast `shouldBe` False

      it "specific JobScope pattern does not match wildcard broadcast (unused case)" $ do
        let broadcast = JobScope Nothing
            pat = JobScope (Just (JobId 123))
        matchesAnyScope [pat] broadcast `shouldBe` False

    describe "multiple patterns" $ do
      it "matches when any pattern in list matches" $ do
        let broadcast = RepoScope (RepoName "my-repo")
            patterns = [RepoScope (RepoName "other-repo"), RepoScope (RepoName "my-repo"), JobScope Nothing]
        matchesAnyScope patterns broadcast `shouldBe` True

      it "does not match when no patterns match" $ do
        let broadcast = RepoScope (RepoName "my-repo")
            patterns = [RepoScope (RepoName "other-repo"), JobScope Nothing]
        matchesAnyScope patterns broadcast `shouldBe` False
