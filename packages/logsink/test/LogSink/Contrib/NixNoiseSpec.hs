{-# LANGUAGE OverloadedStrings #-}

module LogSink.Contrib.NixNoiseSpec (spec) where

import LogSink.Contrib.NixNoise (LineGroup (..), groupNixNoiseLines)
import Test.Hspec

spec :: Spec
spec = describe "LogSink.Contrib.NixNoise" $ do
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

    it "groups ALL consecutive Added/Updated input lines into ONE block (real Nix output)" $ do
      -- Real Nix output format from flake builds
      let warningLine = "warning: not writing modified lock file of flake 'path:/nix/store/wa7hh32h712vm1l2ig2wc9ws1yfgc5jl-devour-flake':"
          noiseLines =
            [ "• Added input 'flake':"
            , "    'git+file:///home/srid/.local/share/vira/state/workspace/vira/jobs/685/project?rev=929e16064b5d602f5571d57b9c2226171cc4ec1a&shallow=1' (2026-01-05)"
            , "• Added input 'flake/co-log-effectful':"
            , "    'github:eldritch-cookie/co-log-effectful/a62ffdad80d11cc513f4e1124b0e07a086fed5fe?narHash=sha256-GCwltDhEhteUxs0RLhcEGPKjvtEKr4nNm9w8QTTNfiA%3D' (2025-10-21)"
            , "• Added input 'flake/flake-parts/nixpkgs-lib':"
            , "    follows 'flake/nixpkgs'"
            , "• Updated input 'systems':"
            , "    'github:srid/empty/23d743284b73ae69caf0cb7874edf05c0c631a1f?narHash=sha256-JeMK8G1oabQTSpqXhYaYtPRak4m6z1xxyRKf8CvHy14%3D' (2024-02-23)"
            , "  → 'path:/nix/store/d7xz40i7v2lpwz3gpdshbpah664l0fyz-source/aarch64-darwin%2Cx86_64-linux?lastModified=1&narHash=sha256-Th5ieTvgLSbatr8qFBiU0Rwe3SzfKaKmX8BVxQiqP74%3D' (1970-01-01)"
            ]
          fetchingLine = "fetching 'git+file:///home/srid/.local/share/vira/state/workspace/vira/jobs/685/project?rev=929e16064b5d602f5571d57b9c2226171cc4ec1a&shallow=1'..."
          input = warningLine : noiseLines ++ [fetchingLine]
      -- ALL noise lines should be in ONE block, not multiple blocks
      groupNixNoiseLines input
        `shouldBe` [ RegularLine warningLine
                   , NixNoiseBlock noiseLines
                   , RegularLine fetchingLine
                   ]
