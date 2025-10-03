-- | Git tool information for ToolsPage
module Vira.Page.ToolsPage.Git (
  tool,
  display,
) where

import Effectful.Git qualified as Git
import Vira.Page.ToolsPage.Core (Tool (..), ToolDisplay (..))

-- | Git tool definition
tool :: Tool
tool =
  Tool
    { name = "Git"
    , description = "Distributed version control system"
    , url = "https://git-scm.com"
    , binPaths = toText Git.git :| []
    }

-- | Git tool display styling
display :: ToolDisplay
display =
  ToolDisplay
    { initial = "G"
    , bgClass = "bg-orange-100"
    , textClass = "text-orange-600"
    }
