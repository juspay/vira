{-# LANGUAGE DuplicateRecordFields #-}

module Vira.CI.Pipeline.ProcessHelpers where

import Data.Text qualified as T
import DevourFlake (DevourFlakeArgs (..), devourFlake)
import Effectful.Process (CreateProcess)
import System.FilePath ((</>))
import System.Nix.Core (nix)
import System.Process (proc)
import Vira.CI.Pipeline.Type (Flake (..))

-- | Create a build process for a flake
createBuildProcess :: Flake -> CreateProcess
createBuildProcess (Flake path overrideInputs) =
  proc nix $ devourFlake args
  where
    args =
      DevourFlakeArgs
        { flakePath = path
        , systems = Nothing
        , outLink = Just (path </> "result")
        , overrideInputs = overrideInputs
        }

{- | Parse devour-flake output to extract store paths
devour-flake with --print-out-paths prints one store path per line
-}
parseDevourFlakeOutput :: Text -> [FilePath]
parseDevourFlakeOutput output =
  lines output
    & filter (T.isPrefixOf "/nix/store/")
    & map toString
