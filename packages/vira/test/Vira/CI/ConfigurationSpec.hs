{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.ConfigurationSpec (spec) where

import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Effectful.Git (BranchName (..), Commit (..), CommitID (..), RepoName (..))
import Paths_vira (getDataFileName)
import Test.Hspec
import Vira.CI.Configuration
import Vira.CI.Context (ViraContext (..))
import Vira.CI.Pipeline.Implementation (defaultPipeline)
import Vira.CI.Pipeline.Type (BuildStage (..), Flake (..), SignoffStage (..), ViraPipeline (..))
import Vira.State.Type (Branch (..))

-- Test data
testBranchStaging :: Branch
testBranchStaging =
  Branch
    { repoName = RepoName "test-repo"
    , branchName = BranchName "staging"
    , headCommit =
        Commit
          { id = CommitID "abc123"
          , message = "Test commit"
          , date = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
          , author = "Test Author"
          , authorEmail = "test@example.com"
          }
    }

testContextStaging :: ViraContext
testContextStaging =
  ViraContext
    { branch = testBranchStaging.branchName
    , dirty = False
    }

spec :: Spec
spec = describe "Vira.CI.Configuration" $ do
  describe "applyConfig" $ do
    it "applies valid config correctly" $ do
      configPath <- getDataFileName "test/sample-configs/simple-example.hs"
      configCode <- decodeUtf8 <$> readFileBS configPath
      result <- applyConfig configCode testContextStaging defaultPipeline
      case result of
        Right pipeline -> do
          pipeline.signoff.enable `shouldBe` True
          let (rootFlake :| _) = pipeline.build.flakes
          rootFlake.path `shouldBe` "."
          rootFlake.overrideInputs `shouldBe` [("local", "github:boolean-option/false")]
        Left err -> expectationFailure $ "Config application failed: " <> show err
