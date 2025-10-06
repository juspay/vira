-- | Cachix tool-specific logic
module Vira.Tool.Tools.Cachix (
  getToolData,
) where

import Effectful (Eff, IOE, (:>))
import Vira.Lib.Cachix qualified as Cachix
import Vira.Tool.Type.ToolData (ToolData (..))

-- | Get Cachix tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData ())
getToolData =
  pure
    ToolData
      { name = "Cachix"
      , description = "Proprietary Nix binary cache hosting service"
      , url = "https://cachix.org"
      , binPaths = one $ toText Cachix.cachixBin
      , status = ()
      }
