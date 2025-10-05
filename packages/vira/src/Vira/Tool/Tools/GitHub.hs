-- | GitHub tool-specific logic
module Vira.Tool.Tools.GitHub (
  getToolData,
) where

import Effectful (Eff, IOE, (:>))
import GH.Auth.Status qualified as GH
import GH.Core qualified as GH
import GH.Signoff qualified as GH
import Vira.Tool.Type (ToolData (..))

-- | Get GitHub tool data with metadata and runtime info
getToolData :: (IOE :> es) => Eff es (ToolData GH.AuthStatus)
getToolData = do
  info <- liftIO GH.checkAuthStatus
  pure
    ToolData
      { name = "GitHub CLI"
      , description = "GitHub command line tool for various operations"
      , url = "https://cli.github.com"
      , binPaths = toText GH.ghBin :| [toText GH.ghSignoffBin]
      , info
      }
