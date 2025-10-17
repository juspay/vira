{-# LANGUAGE TemplateHaskell #-}

module DevourFlake.DevourFlakePath (
  devourFlakePath,
) where

import IncludeEnv.TH (includeEnv)

-- | Path to github:srid/devour-flake flake output
$(includeEnv "DEVOUR_FLAKE_PATH" "devourFlakePath")

devourFlakePath :: FilePath
