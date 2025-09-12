{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Vira.CI.ConfigurationSpec (spec) where

import Data.Map.Strict qualified as Map
import Effectful.Git (BranchName (..), CommitID (..))
import Test.Hspec
import Text.RawString.QQ
import Vira.CI.Configuration
import Vira.CI.Environment (ViraEnvironment (..))
import Vira.CI.Types
import Vira.State.Type (Branch (..), Repo (..), RepoName (..), RepoSettings (..))

spec :: Spec
spec = do
  describe "YAML Configuration Parsing" $ do
    it "parses empty configuration" $ do
      let yaml = "[]"
      case parseViraConfig yaml of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right (ViraConfig rules) -> rules `shouldBe` []

    it "parses basic pipeline configuration" $ do
      let yaml =
            [r|
- pipeline:
    signoff: {}
|]
      case parseViraConfig yaml of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right (ViraConfig [ViraPipelineOverlay condition pipeline]) -> do
          condition `shouldBe` Nothing
          signoff pipeline `shouldBe` Just SignoffStage
          build pipeline `shouldBe` Nothing
        Right _ -> expectationFailure "Expected exactly one config rule"

    it "parses build stage with override inputs" $ do
      let yaml =
            [r|
- pipeline:
    build:
      overrideInputs:
        local: "github:boolean-option/false"
        foo: "bar"
|]
      case parseViraConfig yaml of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right (ViraConfig [ViraPipelineOverlay _ pipeline]) -> do
          let expectedOverrides = Map.fromList [("local", "github:boolean-option/false"), ("foo", "bar")]
          build pipeline `shouldBe` Just (BuildStage (Just expectedOverrides))
        Right _ -> expectationFailure "Expected exactly one config rule"

    it "parses conditional configuration" $ do
      let yaml =
            [r|
- if:
    branch:
      - "main"
      - "release/*"
  pipeline:
    attic: {}
    signoff: {}
|]
      case parseViraConfig yaml of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right (ViraConfig [ViraPipelineOverlay condition pipeline]) -> do
          condition `shouldBe` Just (Condition ["main", "release/*"])
          attic pipeline `shouldBe` Just AtticStage
          signoff pipeline `shouldBe` Just SignoffStage
        Right _ -> expectationFailure "Expected exactly one config rule"

    it "parses multiple rules" $ do
      let yaml =
            [r|
- pipeline:
    signoff: {}
- if:
    branch: ["main"]
  pipeline:
    attic: {}
|]
      case parseViraConfig yaml of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right (ViraConfig rules) -> length rules `shouldBe` 2

  describe "Configuration Application" $ do
    let mockEnv branchName =
          let testRepo = Repo (RepoName "test-repo") "https://github.com/test/test-repo.git" (RepoSettings mempty)
           in ViraEnvironment
                { repo = testRepo
                , Vira.CI.Environment.branch = Branch (RepoName "test-repo") (BranchName branchName) (CommitID "abc123")
                , atticSettings = Nothing
                , cachixSettings = Nothing
                }

    it "applies default configuration to empty pipeline" $ do
      let config =
            ViraConfig
              [ ViraPipelineOverlay Nothing (ViraPipeline Nothing Nothing Nothing (Just SignoffStage))
              ]
          env = mockEnv "feature-branch"
          result = applyConfig env mempty config
      signoff result `shouldBe` Just SignoffStage

    it "applies conditional configuration when branch matches" $ do
      let config =
            ViraConfig
              [ ViraPipelineOverlay Nothing (ViraPipeline Nothing Nothing Nothing (Just SignoffStage))
              , ViraPipelineOverlay
                  (Just (Condition ["main", "release/*"]))
                  (ViraPipeline Nothing (Just AtticStage) Nothing Nothing)
              ]
          env = mockEnv "main"
          result = applyConfig env mempty config
      signoff result `shouldBe` Just SignoffStage
      attic result `shouldBe` Just AtticStage

    it "skips conditional configuration when branch doesn't match" $ do
      let config =
            ViraConfig
              [ ViraPipelineOverlay Nothing (ViraPipeline Nothing Nothing Nothing (Just SignoffStage))
              , ViraPipelineOverlay
                  (Just (Condition ["main"]))
                  (ViraPipeline Nothing (Just AtticStage) Nothing Nothing)
              ]
          env = mockEnv "feature-branch"
          result = applyConfig env mempty config
      signoff result `shouldBe` Just SignoffStage
      attic result `shouldBe` Nothing

    it "later rules override earlier ones (current limitation)" $ do
      let config =
            ViraConfig
              [ ViraPipelineOverlay Nothing (ViraPipeline Nothing Nothing Nothing (Just SignoffStage))
              , ViraPipelineOverlay Nothing (ViraPipeline Nothing Nothing Nothing Nothing)
              ]
          env = mockEnv "main"
          result = applyConfig env mempty config
      -- Current limitation: Nothing in override doesn't disable the stage
      signoff result `shouldBe` Just SignoffStage

  describe "Branch Pattern Matching" $ do
    let mockEnv branchName =
          let testRepo = Repo (RepoName "test-repo") "https://github.com/test/test-repo.git" (RepoSettings mempty)
           in ViraEnvironment
                { repo = testRepo
                , Vira.CI.Environment.branch = Branch (RepoName "test-repo") (BranchName branchName) (CommitID "abc123")
                , atticSettings = Nothing
                , cachixSettings = Nothing
                }

    it "matches exact branch names" $ do
      let condition = Condition ["main"]
          env = mockEnv "main"
      matchesCondition env condition `shouldBe` True

    it "matches wildcard patterns" $ do
      let condition = Condition ["release/*"]
          env1 = mockEnv "release/v1.0"
          env2 = mockEnv "release/beta"
      matchesCondition env1 condition `shouldBe` True
      matchesCondition env2 condition `shouldBe` True

    it "doesn't match non-matching branches" $ do
      let condition = Condition ["main", "release/*"]
          env = mockEnv "feature/new-stuff"
      matchesCondition env condition `shouldBe` False

  describe "Pipeline Merging" $ do
    it "merges pipelines with non-Nothing override fields taking precedence" $ do
      let base = ViraPipeline (Just (BuildStage Nothing)) Nothing Nothing (Just SignoffStage)
          override = ViraPipeline Nothing (Just AtticStage) Nothing Nothing
          result = mergePipelines base override
      build result `shouldBe` Just (BuildStage Nothing) -- kept from base
      attic result `shouldBe` Just AtticStage -- from override
      signoff result `shouldBe` Just SignoffStage -- kept from base
    it "override with Nothing keeps base value (limitation of current design)" $ do
      let base = ViraPipeline Nothing Nothing Nothing (Just SignoffStage)
          override = ViraPipeline Nothing Nothing Nothing Nothing
          result = mergePipelines base override
      -- Current limitation: can't distinguish "not specified" from "explicitly Nothing"
      signoff result `shouldBe` Just SignoffStage
