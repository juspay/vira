{-# LANGUAGE TemplateHaskell #-}

{- | Core GitHub CLI functionality

Provides the path to the gh binary.
-}
module GH.Core (
  ghBin,
) where

import System.Which (staticWhich)

{- | Path to the `gh` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
ghBin :: FilePath
ghBin = $(staticWhich "gh")
