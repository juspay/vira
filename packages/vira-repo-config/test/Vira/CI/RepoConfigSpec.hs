{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.RepoConfigSpec (spec) where

import Effectful.Git (BranchName (..), CommitID (..))
import Paths_vira_repo_config (getDataFileName)
import Test.Hspec
import Vira.CI.Environment.Type (ViraEnvironment (..))
import Vira.CI.Pipeline.Type (AtticStage (..), BuildStage (..), SignoffStage (..), ViraPipeline (..), defaultPipeline)
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

testBranchStaging :: Branch
testBranchStaging =
  Branch
    { repoName = RepoName "test-repo"
    , branchName = BranchName "staging"
    , headCommit = CommitID "abc123"
    }

testEnvStaging :: ViraEnvironment
testEnvStaging =
  ViraEnvironment
    { repo = testRepo
    , branch = testBranchStaging
    , cachixSettings = Just $ CachixSettings "test-cache" "token123"
    , atticSettings = Nothing
    }

spec :: Spec
spec = describe "Vira.CI.RepoConfig" $ do
  describe "applyConfigFromFile" $ do
    it "applies valid config correctly" $ do
      configPath <- getDataFileName "test/sample-configs/simple-example.hs"
      configCode <- decodeUtf8 <$> readFileBS configPath
      result <- applyConfig configCode testEnvStaging (defaultPipeline testEnvStaging)
      case result of
        Right pipeline -> do
          pipeline.attic.atticEnable `shouldBe` False
          pipeline.signoff.signoffEnable `shouldBe` True
          pipeline.build.overrideInputs `shouldBe` [("local", "github:boolean-option/false")]
        Left err -> expectationFailure $ "Config application failed: " <> err
