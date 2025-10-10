module Vira.State.ResetSpec (spec) where

import Data.SafeCopy (Version)
import Test.Hspec
import Vira.State.Reset (versionToInt)

spec :: Spec
spec = describe "Vira.State.Reset" $ do
  describe "versionToInt" $ do
    it "converts version 0 to 0" $ do
      versionToInt (0 :: Version ()) `shouldBe` 0

    it "converts version 1 to 1" $ do
      versionToInt (1 :: Version ()) `shouldBe` 1

    it "converts version 3 to 3" $ do
      versionToInt (3 :: Version ()) `shouldBe` 3

    it "converts version 12 to 12" $ do
      versionToInt (12 :: Version ()) `shouldBe` 12
