{-# LANGUAGE TemplateHaskell #-}

module DevourFlake.NixSystems (
  nixSystemsFlakeFor,
) where

import IncludeEnv.TH (includeEnv)
import System.FilePath ((</>))
import System.Nix.System (System (..))

-- | Base path to @github:srid/nix-systems@ flake output
$(includeEnv "NIX_SYSTEMS_PATH" "nixSystemsPath")

nixSystemsPath :: FilePath

{- | Get the path to a system flake under @NIX_SYSTEMS_PATH@

Returns 'Nothing' if empty list.
For multiple systems, uses comma-separated sorted naming (srid/nix-systems#3).
-}
nixSystemsFlakeFor :: [System] -> Maybe FilePath
nixSystemsFlakeFor = \case
  [] -> Nothing
  systems -> Just $ nixSystemsPath </> systemsToPath (sort systems)
  where
    systemsToPath :: [System] -> FilePath
    systemsToPath syss =
      intercalate "," $ map toString syss
