module Attic.UrlSpec (spec) where

import Attic.Url (parseCacheUrl)
import Test.Hspec

spec :: Spec
spec = do
  describe "parseCacheUrl" $ do
    it "parses a simple cache URL" $ do
      parseCacheUrl "https://cache.nixos.asia/oss"
        `shouldBe` Right ("https://cache.nixos.asia", "oss")

    it "parses a cache URL with multiple path segments" $ do
      parseCacheUrl "https://cache.example.com/foo/bar/mycache"
        `shouldBe` Right ("https://cache.example.com", "mycache")

    it "parses a cache URL with port" $ do
      parseCacheUrl "https://cache.example.com:8080/cache"
        `shouldBe` Right ("https://cache.example.com:8080", "cache")

    it "fails when URL has no path" $ do
      parseCacheUrl "https://cache.example.com"
        `shouldSatisfy` isLeft

    it "fails when URL has empty path segments only" $ do
      parseCacheUrl "https://cache.example.com/"
        `shouldSatisfy` isLeft

    it "ignores trailing slash" $ do
      parseCacheUrl "https://cache.nixos.asia/oss/"
        `shouldBe` Right ("https://cache.nixos.asia", "oss")
