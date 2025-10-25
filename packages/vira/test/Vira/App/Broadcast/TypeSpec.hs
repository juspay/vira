{-# LANGUAGE OverloadedStrings #-}

module Vira.App.Broadcast.TypeSpec (spec) where

import Effectful.Git (RepoName (..))
import System.FilePath ((</>))
import Test.Hspec
import Vira.App.Broadcast.Type (BroadcastScope (..), matchesAnyPattern)
import Vira.State.Type (JobId (..))

spec :: Spec
spec = describe "Vira.App.Broadcast.Type" $ do
  describe "matchesAnyPattern" $ do
    describe "RepoScope pattern matching" $ do
      it "BUG: pattern 'repo/my-repo/*' should match broadcast 'repo/my-repo'" $ do
        let scope = RepoScope (RepoName "my-repo")
            pattern = "repo" </> "my-repo" </> "*"
        matchesAnyPattern [pattern] scope `shouldBe` True

      it "pattern 'repo/my-repo/**' matches broadcast 'repo/my-repo'" $ do
        let scope = RepoScope (RepoName "my-repo")
            pattern = "repo" </> "my-repo" <> "**"
        matchesAnyPattern [pattern] scope `shouldBe` True

      it "exact pattern 'repo/my-repo' matches broadcast 'repo/my-repo'" $ do
        let scope = RepoScope (RepoName "my-repo")
            pattern = "repo" </> "my-repo"
        matchesAnyPattern [pattern] scope `shouldBe` True

      it "wildcard pattern 'repo/*' matches broadcast 'repo/my-repo'" $ do
        let scope = RepoScope (RepoName "my-repo")
            pattern = "repo" </> "*"
        matchesAnyPattern [pattern] scope `shouldBe` True

      it "pattern 'repo/other-repo/*' does not match broadcast 'repo/my-repo'" $ do
        let scope = RepoScope (RepoName "my-repo")
            pattern = "repo" </> "other-repo" </> "*"
        matchesAnyPattern [pattern] scope `shouldBe` False

    describe "JobScope pattern matching" $ do
      it "pattern 'job/123' matches broadcast 'job/123'" $ do
        let scope = JobScope (JobId 123)
            pattern = "job" </> "123"
        matchesAnyPattern [pattern] scope `shouldBe` True

      it "pattern 'job/*' matches broadcast 'job/123'" $ do
        let scope = JobScope (JobId 123)
            pattern = "job" </> "*"
        matchesAnyPattern [pattern] scope `shouldBe` True

      it "pattern 'job/456' does not match broadcast 'job/123'" $ do
        let scope = JobScope (JobId 123)
            pattern = "job" </> "456"
        matchesAnyPattern [pattern] scope `shouldBe` False

    describe "multiple patterns" $ do
      it "matches when any pattern in list matches" $ do
        let scope = RepoScope (RepoName "my-repo")
            patterns = ["repo/other-repo/*", "repo/my-repo", "job/*"]
        matchesAnyPattern patterns scope `shouldBe` True

      it "does not match when no patterns match" $ do
        let scope = RepoScope (RepoName "my-repo")
            patterns = ["repo/other-repo/*", "job/*"]
        matchesAnyPattern patterns scope `shouldBe` False
