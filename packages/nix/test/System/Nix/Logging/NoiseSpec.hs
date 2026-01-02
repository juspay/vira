{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Logging.NoiseSpec (spec) where

import System.Nix.Logging.Noise (LineGroup (..), groupNixNoiseLines)
import Test.Hspec

spec :: Spec
spec = describe "System.Nix.Noise" $ do
  describe "groupNixNoiseLines" $ do
    it "groups consecutive 'Added input' lines with their continuations" $ do
      let input =
            [ "• Added input 'foo'"
            , "    /nix/store/abc-foo"
            , "  → resolved to xyz"
            , "• Added input 'bar'"
            , "    /nix/store/def-bar"
            ]
      groupNixNoiseLines input
        `shouldBe` [NixNoiseBlock input]

    it "does NOT group error output with indentation as noise" $ do
      let input =
            [ "error: Cannot build '/nix/store/abc.drv'."
            , "    Reason: 1 dependency failed."
            , "    Output paths:"
            , "        /nix/store/xyz"
            ]
      -- Error lines should all be regular lines, not grouped as noise
      groupNixNoiseLines input
        `shouldBe` map RegularLine input

    it "separates noise blocks from regular error output" $ do
      let noiseLines =
            [ "• Added input 'foo'"
            , "    /nix/store/abc-foo"
            ]
          errorLines =
            [ "error: Cannot build '/nix/store/abc.drv'."
            , "    Reason: 1 dependency failed."
            ]
          input = noiseLines ++ errorLines
      groupNixNoiseLines input
        `shouldBe` [ NixNoiseBlock noiseLines
                   , RegularLine "error: Cannot build '/nix/store/abc.drv'."
                   , RegularLine "    Reason: 1 dependency failed."
                   ]

    it "handles empty input" $ do
      groupNixNoiseLines [] `shouldBe` []

    it "handles only regular lines" $ do
      let input = ["Building...", "Linking...", "Done"]
      groupNixNoiseLines input
        `shouldBe` map RegularLine input

    it "groups 'Updated input' lines correctly" $ do
      let input =
            [ "• Updated input 'nixpkgs'"
            , "    /nix/store/old -> /nix/store/new"
            ]
      groupNixNoiseLines input
        `shouldBe` [NixNoiseBlock input]
