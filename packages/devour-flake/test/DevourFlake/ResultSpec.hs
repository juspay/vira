{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DevourFlake.ResultSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.Map.Strict qualified as Map
import DevourFlake.Result
import NeatInterpolation (text)
import System.Nix.System (System (..))
import Test.Hspec

parseJSON :: Text -> DevourFlakeResult
parseJSON json = case eitherDecode (encodeUtf8 json) of
  Left err -> error $ toText $ "Failed to parse JSON in test: " <> err
  Right result -> result

spec :: Spec
spec = do
  describe "DevourFlakeResult Semigroup" $ do
    it "merges SystemOutputs when same system appears in both flakes" $ do
      let flake1 =
            parseJSON
              [text|
        {
          "x86_64-linux": {
            "byName": {"pkg1": "/path1"},
            "outPaths": ["/out1"]
          }
        }
      |]
      let flake2 =
            parseJSON
              [text|
        {
          "x86_64-linux": {
            "byName": {"pkg2": "/path2"},
            "outPaths": ["/out2"]
          }
        }
      |]
      let merged = flake1 <> flake2

      case Map.lookup (System "x86_64-linux") merged.systems of
        Nothing -> expectationFailure "x86_64-linux not found"
        Just linux -> do
          Map.size linux.byName `shouldBe` 2
          linux.outPaths `shouldBe` ["/out1", "/out2"]

    it "uses left-biased union for duplicate package names" $ do
      let flake1 =
            parseJSON
              [text|
        {
          "x86_64-linux": {
            "byName": {"common": "/first"},
            "outPaths": ["/out1"]
          }
        }
      |]
      let flake2 =
            parseJSON
              [text|
        {
          "x86_64-linux": {
            "byName": {"common": "/second"},
            "outPaths": ["/out2"]
          }
        }
      |]
      let merged = flake1 <> flake2

      case Map.lookup (System "x86_64-linux") merged.systems of
        Nothing -> expectationFailure "x86_64-linux not found"
        Just linux ->
          Map.lookup "common" linux.byName `shouldBe` Just "/first"

    it "is associative (semigroup law)" $ do
      let r1 =
            parseJSON
              [text|
        {"x86_64-linux": {"byName": {"a": "/1"}, "outPaths": ["/x"]}}
      |]
      let r2 =
            parseJSON
              [text|
        {"x86_64-linux": {"byName": {"b": "/2"}, "outPaths": ["/y"]}}
      |]
      let r3 =
            parseJSON
              [text|
        {"x86_64-linux": {"byName": {"c": "/3"}, "outPaths": ["/z"]}}
      |]

      ((r1 <> r2) <> r3) `shouldBe` (r1 <> (r2 <> r3))
