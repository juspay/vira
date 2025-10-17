-- | Nix configuration and remote builders
module System.Nix.Config (
  module System.Nix.Config.Core,
) where

import System.Nix.Config.Core (NixConfig (..), RemoteBuilder (..), nixConfigShow)
