{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module DevourFlake (
  DevourFlakeArgs (..),
  devourFlake,
  prefetchFlakeInputs,
  devourFlakePath,
  module DevourFlake.Result,
) where

import DevourFlake.NixSystems (nixSystemsFlakeFor)
import DevourFlake.Result
import IncludeEnv.TH (includeEnv)
import System.Nix.System (System (..))

-- | Path to the locally cached @github:srid/devour-flake@ flake
$(includeEnv "DEVOUR_FLAKE_PATH" "devourFlakePath")

devourFlakePath :: FilePath

-- | Generate arguments to @nix@ for running the @devour-flake@ build against given flake.
devourFlake :: (HasCallStack) => DevourFlakeArgs -> [String]
devourFlake args =
  [ "build"
  , devourFlakePath <> "#default"
  , "-L"
  , "--print-out-paths"
  , "--no-write-lock-file"
  ]
    ++ toCliArgs args

-- | Generate arguments to @nix@ for prefetching flake inputs before build
prefetchFlakeInputs :: (HasCallStack) => DevourFlakeArgs -> [String]
prefetchFlakeInputs args =
  ["flake", "prefetch-inputs", devourFlakePath]
    ++ overrideInputArgs args
    ++ nixOptionArgs args
    ++ experimentalFeaturesArgs args

data DevourFlakeArgs = DevourFlakeArgs
  { flakePath :: FilePath
  , systems :: [System]
  , outLink :: Maybe FilePath
  , overrideInputs :: [(Text, Text)]
  , nixOptions :: [(Text, Text)]
  -- ^ @--option key value@ flags passed to the Nix command
  , experimentalFeatures :: [Text]
  -- ^ @--extra-experimental-features@ flags passed to the Nix command
  }
  deriving stock (Eq, Show)

-- | Generate common --override-input arguments
overrideInputArgs :: DevourFlakeArgs -> [String]
overrideInputArgs args =
  concat
    [ ["--override-input", "flake", args.flakePath]
    , concatMap (\(k, v) -> ["--override-input", "flake/" <> toString k, toString v]) args.overrideInputs
    , case nixSystemsFlakeFor args.systems of
        Nothing -> []
        Just systemsFlake -> ["--override-input", "systems", systemsFlake]
    ]

-- | Generate @--option key value@ arguments
nixOptionArgs :: DevourFlakeArgs -> [String]
nixOptionArgs args =
  concatMap (\(k, v) -> ["--option", toString k, toString v]) args.nixOptions

-- | Generate @--extra-experimental-features@ arguments
experimentalFeaturesArgs :: DevourFlakeArgs -> [String]
experimentalFeaturesArgs args =
  concatMap (\f -> ["--extra-experimental-features", toString f]) args.experimentalFeatures

toCliArgs :: DevourFlakeArgs -> [String]
toCliArgs args =
  overrideInputArgs args
    ++ nixOptionArgs args
    ++ experimentalFeaturesArgs args
    ++ maybe ["--no-link"] (\link -> ["--out-link", link]) args.outLink
