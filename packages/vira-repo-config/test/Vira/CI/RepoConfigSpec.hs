{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.RepoConfigSpec (spec) where

import Effectful.Git (BranchName (..), CommitID (..))
import Test.Hspec
import Vira.CI.Environment.Type (ViraEnvironment (..))
import Vira.CI.Pipeline.Type (AtticStage (..), BuildStage (..), CachixStage (..), SignoffStage (..), ViraPipeline (..))
import Vira.CI.RepoConfig
import Vira.State.Type (AtticSettings (..), Branch (..), CachixSettings (..), Repo (..), RepoName (..), RepoSettings (..))

-- Test data
testRepo :: Repo
testRepo =
  Repo
    { name = RepoName "test-repo"
    , cloneUrl = "https://github.com/test/repo.git"
    , settings = RepoSettings {dummy = Nothing}
    }

testBranchMain :: Branch
testBranchMain =
  Branch
    { repoName = RepoName "test-repo"
    , branchName = BranchName "main"
    , headCommit = CommitID "abc123"
    }

testEnvMain :: ViraEnvironment
testEnvMain =
  ViraEnvironment
    { repo = testRepo
    , branch = testBranchMain
    , cachixSettings = Just $ CachixSettings "test-cache" "token123"
    , atticSettings = Nothing
    }

defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage {buildEnable = True, overrideInputs = []}
    , attic = AtticStage {atticEnable = False}
    , cachix = CachixStage {cachixEnable = True}
    , signoff = SignoffStage {signoffEnable = False}
    }

validConfigContent :: Text
validConfigContent = "configureVira env pipeline = pipeline { attic = (attic pipeline) { atticEnable = True } }"

spec :: Spec
spec = describe "Vira.CI.RepoConfig" $ do
  describe "applyConfig" $ do
    it "applies valid config to main branch correctly" $ do
      result <- applyConfig validConfigContent testEnvMain defaultPipeline
      case result of
        Left err -> expectationFailure $ "Expected success but got error: " <> show err
        Right pipeline -> do
          atticEnable (attic pipeline) `shouldBe` True
