-- | Omnix tool-specific logic
module Vira.Tool.Tools.Omnix (
  getToolData,
) where

import Effectful (Eff, IOE, (:>))
import Vira.Lib.Omnix qualified as Omnix
import Vira.Tool.Type (ToolData (..))

-- | Get Omnix tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData ())
getToolData =
  pure
    ToolData
      { name = "Omnix"
      , description = "A tool for building all Nix flake outputs"
      , url = "https://github.com/juspay/omnix"
      , binPaths = one $ toText Omnix.omnixBin
      , info = ()
      }
