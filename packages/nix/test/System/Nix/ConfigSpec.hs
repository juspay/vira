{-# LANGUAGE OverloadedStrings #-}

module System.Nix.ConfigSpec (spec) where

import Data.Map.Strict qualified as Map
import System.Nix.Config
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
