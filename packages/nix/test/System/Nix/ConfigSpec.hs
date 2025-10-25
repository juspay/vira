{-# LANGUAGE OverloadedStrings #-}

module System.Nix.ConfigSpec (spec) where

import Data.Map.Strict qualified as Map
import Effectful (runPureEff)
import Effectful.Error.Static (runErrorNoCallStack)
import System.Nix.Config.Core
import Test.Hspec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "pConfigFile" $ do
    it "parses real nix config show output" $ do
      let input =
            "abort-on-warn = false\n\
            \accept-flake-config = false\n\
            \access-tokens = \n\
            \allow-dirty = true\n\
            \builders = @/home/srid/.config/nix/machines\n\
            \build-users-group = \n\
            \cores = 0\n\
            \max-jobs = 16\n\
            \system = x86_64-linux\n"
      case parse pConfigFile "<test>" input of
        Left err -> expectationFailure $ show err
        Right cfg -> do
          Map.lookup "abort-on-warn" cfg `shouldBe` Just "false"
          Map.lookup "accept-flake-config" cfg `shouldBe` Just "false"
          Map.lookup "allow-dirty" cfg `shouldBe` Just "true"
          Map.lookup "builders" cfg `shouldBe` Just "@/home/srid/.config/nix/machines"
          Map.lookup "cores" cfg `shouldBe` Just "0"
          Map.lookup "max-jobs" cfg `shouldBe` Just "16"
          Map.lookup "system" cfg `shouldBe` Just "x86_64-linux"

    it "handles empty values" $ do
      let input = "access-tokens = \nbuild-users-group = \n"
      case parse pConfigFile "<test>" input of
        Left err -> expectationFailure $ show err
        Right cfg -> do
          Map.lookup "access-tokens" cfg `shouldBe` Just ""
          Map.lookup "build-users-group" cfg `shouldBe` Just ""

    it "handles trailing newlines" $ do
      let input = "foo = bar\nbaz = qux\n\n"
      case parse pConfigFile "<test>" input of
        Left err -> expectationFailure $ show err
        Right cfg -> do
          Map.lookup "foo" cfg `shouldBe` Just "bar"
          Map.lookup "baz" cfg `shouldBe` Just "qux"

  describe "parseNatural" $ do
    it "parses valid Natural numbers" $ do
      let cfg = Map.fromList [("max-jobs", "16"), ("cores", "0")]
          result1 = runPureEff $ runErrorNoCallStack @Text $ parseNatural "max-jobs" cfg
          result2 = runPureEff $ runErrorNoCallStack @Text $ parseNatural "cores" cfg
      result1 `shouldBe` Right 16
      result2 `shouldBe` Right 0

    it "errors on missing field" $ do
      let cfg = Map.empty
          result = runPureEff $ runErrorNoCallStack @Text $ parseNatural "max-jobs" cfg
      result `shouldSatisfy` isLeft

    it "errors on invalid number" $ do
      let cfg = Map.fromList [("max-jobs", "not-a-number")]
          result = runPureEff $ runErrorNoCallStack @Text $ parseNatural "max-jobs" cfg
      result `shouldSatisfy` isLeft

    it "errors on negative number" $ do
      let cfg = Map.fromList [("max-jobs", "-5")]
          result = runPureEff $ runErrorNoCallStack @Text $ parseNatural "max-jobs" cfg
      result `shouldSatisfy` isLeft

  describe "parseSpaceSeparated" $ do
    it "parses space-separated lists" $ do
      let cfg = Map.fromList [("substituters", "https://cache.nixos.org https://cache.garnix.io")]
      parseSpaceSeparated "substituters" cfg `shouldBe` ["https://cache.nixos.org", "https://cache.garnix.io"]

    it "handles empty values" $ do
      let cfg = Map.fromList [("substituters", "")]
      parseSpaceSeparated "substituters" cfg `shouldBe` []

    it "handles missing fields" $ do
      let cfg = Map.empty
      parseSpaceSeparated "substituters" cfg `shouldBe` []

    it "filters out empty strings" $ do
      let cfg = Map.fromList [("features", "flakes  nix-command")]
      parseSpaceSeparated "features" cfg `shouldBe` ["flakes", "nix-command"]
