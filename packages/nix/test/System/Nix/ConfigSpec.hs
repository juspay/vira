{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Nix.ConfigSpec (spec) where

import Data.Aeson qualified as Aeson
import System.Nix.Config.Core (NixConfigField (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "NixConfigField FromJSON" $ do
    it "parses config field with value and description" $ do
      let json =
            "{\
            \  \"value\": 42,\
            \  \"description\": \"Maximum parallel jobs\"\
            \}"
          result = Aeson.eitherDecode @(NixConfigField Natural) json
      case result of
        Left err -> expectationFailure err
        Right (NixConfigField {value, description}) -> do
          value `shouldBe` 42
          description `shouldBe` "Maximum parallel jobs"

    it "parses config field with list value" $ do
      let json =
            "{\
            \  \"value\": [\"https://cache.nixos.org\", \"https://cache.garnix.io\"],\
            \  \"description\": \"List of substituters\"\
            \}"
          result = Aeson.eitherDecode @(NixConfigField [Text]) json
      case result of
        Left err -> expectationFailure err
        Right (NixConfigField {value, description}) -> do
          value `shouldBe` ["https://cache.nixos.org", "https://cache.garnix.io"]
          description `shouldBe` "List of substituters"

  describe "parseBuilderValue" $ do
    it
      "parses empty string"
      -- Note: parseBuilderValue is effectful, so we skip testing it here
      -- Integration tests in nixConfigShow will cover this
      pending
