{-# LANGUAGE TemplateHaskell #-}

module DevourFlake.NixSystems (
  nixSystemsPath,
) where

import IncludeEnv.TH (includeEnv)

-- | Path to github:srid/nix-systems flake output
$(includeEnv "NIX_SYSTEMS_PATH" "nixSystemsPath")

nixSystemsPath :: FilePath
