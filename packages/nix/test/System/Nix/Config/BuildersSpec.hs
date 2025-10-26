{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Config.BuildersSpec (spec) where

import System.Nix.Config.Builders
import Test.Hspec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "pBuilders" $ do
    it "parses real machines file format" $ do
      let input =
            "ssh-ng://srid@192.168.2.247 aarch64-darwin /home/srid/.ssh/id_ed25519 16 2 benchmark,big-parallel - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSU82S2Q2OG5LdFlqNVhTaWgveVlteE96M2o0WUdMUGQxUTE1cTF0dUdsZWUgCg==\n"
      case parse pBuilders "<test>" input of
        Left err -> expectationFailure $ show err
        Right builders -> do
          length builders `shouldBe` 1
          case viaNonEmpty head builders of
            Nothing -> expectationFailure "Expected at least one builder"
            Just builder -> do
              builder.uri `shouldBe` "ssh-ng://srid@192.168.2.247"
              builder.platforms `shouldBe` ["aarch64-darwin"]
              builder.sshKey `shouldBe` Just "/home/srid/.ssh/id_ed25519"
              builder.maxJobs `shouldBe` 16
              builder.speedFactor `shouldBe` 2
              builder.supportedFeatures `shouldBe` ["benchmark", "big-parallel"]
              builder.mandatoryFeatures `shouldBe` []
              builder.publicHostKey
                `shouldBe` Just "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSU82S2Q2OG5LdFlqNVhTaWgveVlteE96M2o0WUdMUGQxUTE1cTF0dUdsZWUgCg=="

    it "handles optional fields with dash" $ do
      let input = "ssh://host x86_64-linux - 1 1 - - -\n"
      case parse pBuilders "<test>" input of
        Left err -> expectationFailure $ show err
        Right builders -> do
          length builders `shouldBe` 1
          case viaNonEmpty head builders of
            Nothing -> expectationFailure "Expected at least one builder"
            Just builder -> do
              builder.sshKey `shouldBe` Nothing
              builder.supportedFeatures `shouldBe` []
              builder.mandatoryFeatures `shouldBe` []
              builder.publicHostKey `shouldBe` Nothing
