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
import Vira.CI.Pipeline.Type (BuildStage (..), Flake (..), NixConfig (..), SignoffStage (..), ViraPipeline (..), validateExperimentalFeatures, validateNixOptions)
import Vira.State.Type (Branch (..))
import Prelude hiding (id)

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
    , deleted = False
    }

testContextStaging :: ViraContext
testContextStaging =
  ViraContext
    { branch = testBranchStaging.branchName
    , onlyBuild = False
    , commitId = testBranchStaging.headCommit.id
    , cloneUrl = "https://example.com/test-repo.git"
    , repoDir = "/tmp/test-repo"
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

    it "applies nix config correctly" $ do
      configPath <- getDataFileName "test/sample-configs/nix-options-example.hs"
      configCode <- decodeUtf8 <$> readFileBS configPath
      result <- applyConfig configCode testContextStaging defaultPipeline
      case result of
        Right pipeline -> do
          pipeline.nix.options
            `shouldBe` ( [ ("sandbox", "relaxed")
                         , ("cores", "4")
                         , ("max-jobs", "2")
                         , ("allow-import-from-derivation", "true")
                         ] ::
                           [(Text, Text)]
                       )
          pipeline.nix.experimentalFeatures
            `shouldBe` (["impure-derivations"] :: [Text])
        Left err -> expectationFailure $ "Config application failed: " <> show err

  describe "validateNixOptions" $ do
    it "accepts all whitelisted options" $ do
      let opts = [("sandbox", "relaxed"), ("cores", "4"), ("max-jobs", "2"), ("allow-import-from-derivation", "true")]
      validateNixOptions opts `shouldBe` []

    it "rejects disallowed options" $ do
      let opts = [("sandbox", "relaxed"), ("access-tokens", "secret")]
      validateNixOptions opts `shouldBe` ["access-tokens"]

    it "returns empty for empty options" $ do
      validateNixOptions [] `shouldBe` []

  describe "validateExperimentalFeatures" $ do
    it "accepts whitelisted features" $ do
      validateExperimentalFeatures ["impure-derivations", "ca-derivations"] `shouldBe` []

    it "rejects unknown features" $ do
      validateExperimentalFeatures ["impure-derivations", "flakes"] `shouldBe` ["flakes"]

    it "returns empty for empty list" $ do
      validateExperimentalFeatures [] `shouldBe` []
