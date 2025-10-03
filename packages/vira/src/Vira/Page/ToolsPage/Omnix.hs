-- | Omnix tool information for ToolsPage
module Vira.Page.ToolsPage.Omnix (
  tool,
  display,
) where

import Vira.Lib.Omnix qualified as Omnix
import Vira.Page.ToolsPage.Core (Tool (..), ToolDisplay (..))

-- | Omnix tool definition
tool :: Tool
tool =
  Tool
    { name = "Omnix"
    , description = "A tool for building all Nix flake outputs"
    , url = "https://github.com/juspay/omnix"
    , binPaths = toText Omnix.omnixBin :| []
    }

-- | Omnix tool display styling
display :: ToolDisplay
display =
  ToolDisplay
    { initial = "O"
    , bgClass = "bg-purple-100"
    , textClass = "text-purple-600"
    }
