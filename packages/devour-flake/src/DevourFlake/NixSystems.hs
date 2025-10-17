{-# LANGUAGE TemplateHaskell #-}

module DevourFlake.NixSystems (
  nixSystemsFlakeFor,
) where

import IncludeEnv.TH (includeEnv)
import System.FilePath ((</>))
import System.Nix.System (System (..))

-- | Base path to github:srid/nix-systems flake output
$(includeEnv "NIX_SYSTEMS_PATH" "nixSystemsPath")

nixSystemsPath :: FilePath

-- | Get the path to a specific system's flake under NIX_SYSTEMS_PATH
nixSystemsFlakeFor :: System -> FilePath
nixSystemsFlakeFor system = nixSystemsPath </> toString system
