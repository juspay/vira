{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DevourFlake (
  DevourFlakeArgs (..),
  devourFlake,
) where

import DevourFlake.DevourFlakePath (devourFlakePath)
import DevourFlake.NixSystems (nixSystemsFlakeFor)
import System.Nix.System (System (..))

data DevourFlakeArgs = DevourFlakeArgs
  { system :: System
  , outLink :: Maybe FilePath
  , flakePath :: FilePath
  , overrideInputs :: [(Text, Text)]
  }
  deriving stock (Eq, Show)

{- | Generate arguments to pass to CreateProcess for devour-flake
Roughly corresponds to:
nix build github:srid/devour-flake -L --no-link --print-out-paths --override-input flake <flakePath>
-}
devourFlake :: DevourFlakeArgs -> [String]
devourFlake args =
  [ "build"
  , devourFlakePath <> "#json"
  , "-L"
  , "--print-out-paths"
  , -- To suppress 'override input' verbosity and lock file warnings
    -- Disabling it doesn't have effect on CI logs
    -- "--quiet"
    -- "--quiet"
    "--no-write-lock-file"
  , "--override-input"
  , "flake"
  , args.flakePath
  , "--override-input"
  , "systems"
  , nixSystemsFlakeFor args.system
  ]
    <> maybe ["--no-link"] (\link -> ["--out-link", link]) args.outLink
    <> concatMap (\(k, v) -> ["--override-input", "flake/" <> toString k, toString v]) args.overrideInputs
