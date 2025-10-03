-- | Cachix tool information for ToolsPage
module Vira.Page.ToolsPage.Cachix (
  tool,
  display,
) where

import Vira.Lib.Cachix qualified as Cachix
import Vira.Page.ToolsPage.Core (Tool (..), ToolDisplay (..))

-- | Cachix tool definition
tool :: Tool
tool =
  Tool
    { name = "Cachix"
    , description = "Proprietary Nix binary cache hosting service"
    , url = "https://cachix.org"
    , binPaths = toText Cachix.cachixBin :| []
    }

-- | Cachix tool display styling
display :: ToolDisplay
display =
  ToolDisplay
    { initial = "C"
    , bgClass = "bg-blue-100"
    , textClass = "text-blue-600"
    }
