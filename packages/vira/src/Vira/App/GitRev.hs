{- |
Git revision information injected at build time.

This file is overwritten during Nix builds with the actual git commit hash.
For local development, it shows "dev" to indicate development mode.
-}
module Vira.App.GitRev (
  gitHashFull,
  gitHashShort,
) where

{- | Full git commit hash (40 characters)
Overwritten during Nix build with actual hash
-}
gitHashFull :: Text
gitHashFull = "dev"

{- | Short git commit hash (7 characters)
Overwritten during Nix build with actual hash
-}
gitHashShort :: Text
gitHashShort = "dev"
