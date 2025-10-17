{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module DevourFlake (
  DevourFlakeArgs (..),
  devourFlake,
) where

import IncludeEnv.TH (includeEnv)
import System.Nix.System (System (..))

-- | Path to github:srid/devour-flake flake output
$(includeEnv "DEVOUR_FLAKE_PATH" "devourFlakePath")

devourFlakePath :: FilePath

data DevourFlakeArgs = DevourFlakeArgs
  { systems :: Maybe (NonEmpty System)
  , outLink :: Maybe FilePath
  , flakePath :: FilePath
  , overrideInputs :: [(Text, Text)]
  }
  deriving stock (Eq, Show)

{- | Generate arguments to pass to CreateProcess for devour-flake
Roughly corresponds to:
nix build github:srid/devour-flake -L --no-link --print-out-paths --override-input flake <flakePath>
-}
devourFlake :: (HasCallStack) => DevourFlakeArgs -> [String]
devourFlake args =
  [ "build"
  , devourFlakePath <> "#json"
  , "-L"
  , "--print-out-paths"
  , "--no-write-lock-file"
  , "--override-input"
  , "flake"
  , args.flakePath
  ]
    <> maybe ["--no-link"] (\link -> ["--out-link", link]) args.outLink
    <> concatMap (\(k, v) -> ["--override-input", "flake/" <> toString k, toString v]) args.overrideInputs
    <> case args.systems of
      Nothing -> []
      Just _systemsList ->
        error "Not implemented"
