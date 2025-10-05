module Attic.UrlSpec (spec) where

import Attic.Types (AtticCache (..), AtticServerEndpoint (..))
import Attic.Url (parseCacheUrl)
import Test.Hspec

spec :: Spec
spec = do
  describe "parseCacheUrl" $ do
    it "parses a simple cache URL" $ do
      parseCacheUrl "https://cache.nixos.asia/oss"
        `shouldBe` Right (AtticServerEndpoint "https://cache.nixos.asia", AtticCache "oss")

    it "fails when URL has multiple path segments" $ do
      parseCacheUrl "https://cache.example.com/foo/bar/mycache"
        `shouldSatisfy` isLeft

    it "parses a cache URL with port" $ do
      parseCacheUrl "https://cache.example.com:8080/cache"
        `shouldBe` Right (AtticServerEndpoint "https://cache.example.com:8080", AtticCache "cache")

    it "fails when URL has no path" $ do
      parseCacheUrl "https://cache.example.com"
        `shouldSatisfy` isLeft

    it "ignores trailing slash" $ do
      parseCacheUrl "https://cache.nixos.asia/oss/"
        `shouldBe` Right (AtticServerEndpoint "https://cache.nixos.asia", AtticCache "oss")
