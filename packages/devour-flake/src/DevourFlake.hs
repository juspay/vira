{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module DevourFlake (
  DevourFlakeArgs (..),
  devourFlake,
  devourFlakePath,
  module DevourFlake.Result,
) where

import DevourFlake.NixSystems (nixSystemsFlakeFor)
import DevourFlake.Result
import IncludeEnv.TH (includeEnv)
import System.Nix.System (System (..))

-- | Path to the locally cached github:srid/devour-flake flake
$(includeEnv "DEVOUR_FLAKE_PATH" "devourFlakePath")

devourFlakePath :: FilePath

-- | Generate arguments to `nix` for running the devour-flake build against given flake.
devourFlake :: (HasCallStack) => DevourFlakeArgs -> [String]
devourFlake args =
  [ "build"
  , devourFlakePath <> "#default"
  , "-L"
  , "--print-out-paths"
  , "--no-write-lock-file"
  ]
    ++ toCliArgs args

data DevourFlakeArgs = DevourFlakeArgs
  { flakePath :: FilePath
  , systems :: [System]
  , outLink :: Maybe FilePath
  , overrideInputs :: [(Text, Text)]
  }
  deriving stock (Eq, Show)

toCliArgs :: DevourFlakeArgs -> [String]
toCliArgs args =
  concat
    [ ["--override-input", "flake", args.flakePath]
    , maybe ["--no-link"] (\link -> ["--out-link", link]) args.outLink
    , concatMap (\(k, v) -> ["--override-input", "flake/" <> toString k, toString v]) args.overrideInputs
    , case nixSystemsFlakeFor args.systems of
        Nothing -> []
        Just systemsFlake -> ["--override-input", "systems", systemsFlake]
    ]
