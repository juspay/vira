-- | Git tool-specific logic
module Vira.Tool.Tools.Git (
  getToolData,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Git qualified as Git
import Vira.Tool.Type.ToolData (ToolData (..))

-- | Get Git tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData ())
getToolData =
  pure
    ToolData
      { name = "Git"
      , url = "https://git-scm.com"
      , binPaths = one $ toText Git.git
      , status = ()
      }
