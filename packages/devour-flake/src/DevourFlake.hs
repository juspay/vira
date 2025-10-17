{-# LANGUAGE OverloadedRecordDot #-}

module DevourFlake (
  devourFlake,
) where

import DevourFlake.DevourFlakePath (devourFlakePath)
import DevourFlake.NixSystems (nixSystemsFlakeFor)
import System.Nix.System (System (..))

data DevourFlakeArgs = DevourFlakeArgs
  { system :: System
  , outLink :: Maybe FilePath
  , flakePath :: FilePath
  }
  deriving stock (Eq, Show)

{- | Generate arguments to pass to CreateProcess for devour-flake
Roughly corresponds to:
nix build github:srid/devour-flake -L --no-link --print-out-paths --override-input flake <flakePath>
-}
devourFlake :: DevourFlakeArgs -> [String]
devourFlake args =
  [ "build"
  , devourFlakePath
  , "-L"
  , "--print-out-paths"
  , -- To suppress 'override input' verbosity
    "--quiet"
  , "--quiet"
  , "--override-input"
  , "flake"
  , args.flakePath
  , "--override-input"
  , "systems"
  , nixSystemsFlakeFor args.system
  ]
    <> maybe ["--no-link"] (\link -> ["--out-link", link]) args.outLink
