{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Nix.RemoteBuilderResolverSpec (spec) where

import System.Nix.Config.Builders (RemoteBuilder (..))
import System.Nix.System (System (..))
import Test.Hspec
import Vira.CI.Nix.RemoteBuilderResolver (BuildTarget (..), matchSystemToBuilder)

spec :: Spec
spec = describe "Vira.CI.Nix.RemoteBuilderResolver" $ do
  describe "matchSystemToBuilder" $ do
    it "matches system to builder that supports it" $ do
      let builders = [darwinBuilder, linuxBuilder]
      matchSystemToBuilder builders (System "aarch64-darwin")
        `shouldBe` Just darwinBuilder
      matchSystemToBuilder builders (System "x86_64-linux")
        `shouldBe` Just linuxBuilder

    it "returns Nothing for unmatchable system" $ do
      let builders = [darwinBuilder]
      matchSystemToBuilder builders (System "x86_64-linux")
        `shouldBe` Nothing

    it "handles empty builder list" $ do
      matchSystemToBuilder [] (System "x86_64-linux")
        `shouldBe` Nothing

    it "returns first matching builder when multiple match" $ do
      let builders = [darwinBuilder, anotherDarwinBuilder]
      matchSystemToBuilder builders (System "aarch64-darwin")
        `shouldBe` Just darwinBuilder

  describe "BuildTarget" $ do
    it "LocalBuild contains the system" $ do
      let target = LocalBuild (System "x86_64-linux")
      case target of
        LocalBuild sys -> sys `shouldBe` System "x86_64-linux"
        RemoteBuild _ _ -> expectationFailure "Expected LocalBuild"

    it "RemoteBuild contains system and builder" $ do
      let target = RemoteBuild (System "aarch64-darwin") darwinBuilder
      case target of
        RemoteBuild sys builder -> do
          sys `shouldBe` System "aarch64-darwin"
          builder.uri `shouldBe` "ssh-ng://darwin-builder"
        LocalBuild _ -> expectationFailure "Expected RemoteBuild"

-- Test fixtures

darwinBuilder :: RemoteBuilder
darwinBuilder =
  RemoteBuilder
    { uri = "ssh-ng://darwin-builder"
    , platforms = [System "aarch64-darwin", System "x86_64-darwin"]
    , sshKey = Nothing
    , maxJobs = 4
    , speedFactor = 1
    , supportedFeatures = []
    , mandatoryFeatures = []
    , publicHostKey = Nothing
    }

anotherDarwinBuilder :: RemoteBuilder
anotherDarwinBuilder =
  RemoteBuilder
    { uri = "ssh-ng://darwin-builder-2"
    , platforms = [System "aarch64-darwin"]
    , sshKey = Nothing
    , maxJobs = 2
    , speedFactor = 1
    , supportedFeatures = []
    , mandatoryFeatures = []
    , publicHostKey = Nothing
    }

linuxBuilder :: RemoteBuilder
linuxBuilder =
  RemoteBuilder
    { uri = "ssh-ng://linux-builder"
    , platforms = [System "x86_64-linux", System "aarch64-linux"]
    , sshKey = Nothing
    , maxJobs = 8
    , speedFactor = 2
    , supportedFeatures = ["big-parallel"]
    , mandatoryFeatures = []
    , publicHostKey = Nothing
    }
