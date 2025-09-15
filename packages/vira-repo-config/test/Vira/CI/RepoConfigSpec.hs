{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.RepoConfigSpec (spec) where

import Effectful.Git (BranchName (..), CommitID (..))
import Paths_vira_repo_config (getDataFileName)
import Test.Hspec
import Vira.CI.Environment.Type (ViraEnvironment (..))
import Vira.CI.Pipeline.Type (AtticStage (..), BuildStage (..), CachixStage (..), SignoffStage (..), ViraPipeline (..))
import Vira.CI.RepoConfig
import Vira.State.Type (Branch (..), CachixSettings (..), Repo (..), RepoName (..), RepoSettings (..))

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

spec :: Spec
spec = describe "Vira.CI.RepoConfig" $ do
  describe "applyConfigFromFile" $ do
    it "applies valid config correctly" $ do
      configPath <- getDataFileName "test/sample-configs/simple-example.hs"
      configCode <- decodeUtf8 <$> readFileBS configPath
      result <- applyConfig configCode testEnvMain defaultPipeline
      case result of
        Right pipeline -> do
          pipeline.attic.atticEnable `shouldBe` True
          pipeline.signoff.signoffEnable `shouldBe` False
          pipeline.build.overrideInputs `shouldBe` []
        Left err -> expectationFailure $ "Config application failed: " <> err
