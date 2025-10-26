{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Nix.ConfigSpec (spec) where

import Data.Aeson qualified as Aeson
import System.Nix.Config.Core (Builders (..), NixConfigField (..))
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

  describe "Builders FromJSON" $ do
    it "parses empty string as BuildersEmpty" $ do
      let result = Aeson.eitherDecode @Builders "\"\""
      result `shouldBe` Right BuildersEmpty

    it "parses file reference" $ do
      let result = Aeson.eitherDecode @Builders "\"@/home/user/.config/nix/machines\""
      result `shouldBe` Right (BuildersFile "/home/user/.config/nix/machines")

    it "parses inline builders" $ do
      let json = "\"ssh://builder x86_64-linux - 4 1 - - -\""
          result = Aeson.eitherDecode @Builders json
      case result of
        Left err -> expectationFailure err
        Right (BuildersList bs) -> length bs `shouldBe` 1
        Right other -> expectationFailure $ "Expected BuildersList but got: " <> show other
