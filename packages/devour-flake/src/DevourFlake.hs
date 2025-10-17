module DevourFlake (
  devourFlake,
) where

import DevourFlake.DevourFlakePath (devourFlakePath)
import System.Nix.System (System (..))

{- | Generate arguments to pass to CreateProcess for devour-flake
Roughly corresponds to:
nix build github:srid/devour-flake -L --no-link --print-out-paths --override-input flake <flakePath>
-}
devourFlake :: System -> FilePath -> [String]
devourFlake system flakePath =
  [ "build"
  , devourFlakePath
  , "-L"
  , "--no-link"
  , "--print-out-paths"
  , "--quiet"
  , "--quiet"
  , "--override-input"
  , "flake"
  , flakePath
  , "--system"
  , toString system
  ]
