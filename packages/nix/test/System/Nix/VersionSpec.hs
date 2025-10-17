{-# LANGUAGE OverloadedStrings #-}

module System.Nix.VersionSpec (spec) where

import System.Nix.Version (NixVersion (..), parseVersion)
import Test.Hspec

spec :: Spec
spec = describe "System.Nix.Version" $ do
  describe "parseVersion" $ do
    it "parses standard nix version output" $ do
      parseVersion "nix (Nix) 2.18.1" `shouldBe` Right (NixVersion "2.18.1")

    it "parses version with two components" $ do
      parseVersion "nix (Nix) 2.18" `shouldBe` Right (NixVersion "2.18")

    it "parses version with different prefix" $ do
      parseVersion "nix 2.22.0" `shouldBe` Right (NixVersion "2.22.0")

    it "fails on invalid input" $ do
      case parseVersion "not a version" of
        Left _ -> pass
        Right v -> expectationFailure $ "Expected parse failure but got: " <> show v

    it "fails on empty input" $ do
      case parseVersion "" of
        Left _ -> pass
        Right v -> expectationFailure $ "Expected parse failure but got: " <> show v
