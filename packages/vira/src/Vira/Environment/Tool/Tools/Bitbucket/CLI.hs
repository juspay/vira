{-# LANGUAGE TemplateHaskell #-}

{- | Bitbucket CLI executable path

Provides the bb executable path.
-}
module Vira.Environment.Tool.Tools.Bitbucket.CLI (
  bbBin,
) where

import System.Which (staticWhich)

{- | Path to the @bb@ executable

This should be available in the PATH, thanks to Nix and 'System.Which.staticWhich'.
-}
bbBin :: FilePath
bbBin = $(staticWhich "bb")
