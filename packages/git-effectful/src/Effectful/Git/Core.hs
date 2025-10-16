{-# LANGUAGE TemplateHaskell #-}

{- | Core git executable path

Provides the git executable path for other modules.
-}
module Effectful.Git.Core (
  git,
) where

import System.Which (staticWhich)

{- | Path to the `git` executable

This should be available in the PATH, thanks to Nix and `which` library.
-}
git :: FilePath
git = $(staticWhich "git")
