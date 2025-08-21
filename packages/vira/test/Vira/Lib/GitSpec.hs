module Vira.Lib.GitSpec where

import Data.Map.Strict qualified as Map
import Test.Hspec
import Vira.Lib.Git

spec :: Spec
spec = do
  describe "Git" $ do
    it "remoteBranches" $ do
      let archivedRepo = "https://github.com/srid/leptos-nix-template" -- Archived; won't change
      branches <- remoteBranches archivedRepo
      Map.lookup "main" branches `shouldBe` Just "68506f5bf0a5883e737c0f8b7bab4c651a0d5fc0"
