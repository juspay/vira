{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.ConfigurationSpec (spec) where

import Effectful.Git (BranchName (..), CommitID (..), RepoName (..))
import Paths_vira (getDataFileName)
import Test.Hspec
import Vira.CI.Configuration
import Vira.CI.Environment (ViraEnvironment (..), viraContext)
import Vira.CI.Pipeline (defaultPipeline)
import Vira.CI.Pipeline.Type (AtticStage (..), BuildStage (..), SignoffStage (..), ViraPipeline (..))
import Vira.State.Type (Branch (..), CachixSettings (..), Repo (..))

-- Test data
testRepo :: Repo
testRepo =
  Repo
    { name = RepoName "test-repo"
    , cloneUrl = "https://github.com/test/repo.git"
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
    , workspacePath = "/tmp/test-workspace"
    }

spec :: Spec
spec = describe "Vira.CI.Configuration" $ do
  describe "applyConfig" $ do
    it "applies valid config correctly" $ do
      configPath <- getDataFileName "test/sample-configs/simple-example.hs"
      configCode <- decodeUtf8 <$> readFileBS configPath
      result <- applyConfig configCode (viraContext testEnvStaging) (defaultPipeline testEnvStaging)
      case result of
        Right pipeline -> do
          pipeline.attic.enable `shouldBe` False
          pipeline.signoff.enable `shouldBe` True
          pipeline.build.overrideInputs `shouldBe` [("local", "github:boolean-option/false")]
        Left err -> expectationFailure $ "Config application failed: " <> show err
