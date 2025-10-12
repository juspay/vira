{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vira.CI.ConfigurationSpec (spec) where

import Attic.Config (AtticConfig (..))
import Data.Map.Strict qualified as Map
import Effectful.Git (BranchName (..), CommitID (..), RepoName (..))
import GH.Auth.Status (AuthStatus (..))
import Paths_vira (getDataFileName)
import Test.Hspec
import Vira.CI.Configuration
import Vira.CI.Environment (ViraEnvironment (..), viraContext)
import Vira.CI.Pipeline (defaultPipeline)
import Vira.CI.Pipeline.Type (BuildStage (..), SignoffStage (..), ViraPipeline (..))
import Vira.State.Type (Branch (..), Repo (..))
import Vira.Tool.Type.ToolData qualified as Tool
import Vira.Tool.Type.Tools qualified as Tool

-- Test data
testRepo :: Repo
testRepo =
  let name = RepoName "test-repo"
      cloneUrl = "https://github.com/test/repo.git"
      lastRefresh = Nothing
   in Repo {..}

testBranchStaging :: Branch
testBranchStaging =
  Branch
    { repoName = RepoName "test-repo"
    , branchName = BranchName "staging"
    , headCommit = CommitID "abc123"
    }

-- Empty test tools
testTools :: Tool.Tools
testTools =
  Tool.Tools
    { Tool.attic =
        Tool.ToolData
          { Tool.name = "Attic"
          , Tool.description = "Test tool"
          , Tool.url = "https://example.com"
          , Tool.binPaths = one "test-bin"
          , Tool.status = Right AtticConfig {defaultServer = Nothing, servers = Map.empty}
          }
    , Tool.github =
        Tool.ToolData
          { Tool.name = "GitHub"
          , Tool.description = "Test tool"
          , Tool.url = "https://example.com"
          , Tool.binPaths = one "test-bin"
          , Tool.status = NotAuthenticated
          }
    , Tool.omnix =
        Tool.ToolData
          { Tool.name = "Omnix"
          , Tool.description = "Test tool"
          , Tool.url = "https://example.com"
          , Tool.binPaths = one "test-bin"
          , Tool.status = ()
          }
    , Tool.git =
        Tool.ToolData
          { Tool.name = "Git"
          , Tool.description = "Test tool"
          , Tool.url = "https://example.com"
          , Tool.binPaths = one "test-bin"
          , Tool.status = ()
          }
    , Tool.cachix =
        Tool.ToolData
          { Tool.name = "Cachix"
          , Tool.description = "Test tool"
          , Tool.url = "https://example.com"
          , Tool.binPaths = one "test-bin"
          , Tool.status = ()
          }
    }

testEnvStaging :: ViraEnvironment
testEnvStaging =
  ViraEnvironment
    { repo = testRepo
    , branch = testBranchStaging
    , tools = testTools
    , workspacePath = "/tmp/test-workspace"
    }

spec :: Spec
spec = describe "Vira.CI.Configuration" $ do
  describe "applyConfig" $ do
    it "applies valid config correctly" $ do
      configPath <- getDataFileName "test/sample-configs/simple-example.hs"
      configCode <- decodeUtf8 <$> readFileBS configPath
      result <- applyConfig configCode (viraContext testEnvStaging) defaultPipeline
      case result of
        Right pipeline -> do
          pipeline.signoff.enable `shouldBe` True
          pipeline.build.overrideInputs `shouldBe` [("local", "github:boolean-option/false")]
        Left err -> expectationFailure $ "Config application failed: " <> show err
