{-# LANGUAGE TemplateHaskell #-}

{- | Core nix executable path

Provides the nix executable path for other modules.
-}
module System.Nix.Core (
  nix,
) where

import System.Which (staticWhich)

{- | Path to the `nix` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
nix :: FilePath
nix = $(staticWhich "nix")
