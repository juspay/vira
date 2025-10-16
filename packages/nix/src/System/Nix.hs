-- | Haskell wrapper for the Nix CLI
module System.Nix (
  module System.Nix.Flake.Develop,
  module System.Nix.System,
) where

import System.Nix.Flake.Develop
import System.Nix.System
