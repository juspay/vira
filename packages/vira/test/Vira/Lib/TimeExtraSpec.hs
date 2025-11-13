{-# LANGUAGE OverloadedStrings #-}

module Vira.Lib.TimeExtraSpec (spec) where

import Test.Hspec
import Vira.Lib.TimeExtra (formatDuration)

spec :: Spec
spec = describe "Vira.Lib.TimeExtra" $ do
  describe "formatDuration" $ do
    it "formats seconds only" $ do
      formatDuration 45 `shouldBe` "45s"
      formatDuration 0 `shouldBe` "0s"
      formatDuration 59 `shouldBe` "59s"

    it "formats minutes and seconds" $ do
      formatDuration (2 * 60 + 34) `shouldBe` "2m 34s"
      formatDuration (1 * 60) `shouldBe` "1m 0s"
      formatDuration (59 * 60 + 59) `shouldBe` "59m 59s"

    it "formats hours, minutes, and seconds" $ do
      formatDuration (1 * 3600 + 15 * 60 + 30) `shouldBe` "1h 15m 30s"
      formatDuration (23 * 3600 + 59 * 60 + 59) `shouldBe` "23h 59m 59s"
      formatDuration (5 * 3600) `shouldBe` "5h 0m 0s"

    it "formats days, hours, minutes, and seconds" $ do
      formatDuration (3 * 86400 + 2 * 3600 + 15 * 60 + 30) `shouldBe` "3d 2h 15m 30s"
      formatDuration (1 * 86400) `shouldBe` "1d 0h 0m 0s"
      formatDuration (7 * 86400 + 23 * 3600 + 59 * 60 + 45) `shouldBe` "7d 23h 59m 45s"
      -- Test the 73h case mentioned by user (should be ~3 days)
      formatDuration (73 * 3600 + 28 * 60 + 36) `shouldBe` "3d 1h 28m 36s"
