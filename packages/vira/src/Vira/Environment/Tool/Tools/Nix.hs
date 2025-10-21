-- | Nix tool-specific logic
module Vira.Environment.Tool.Tools.Nix (
  getToolData,
  viewToolStatus,
) where

import DevourFlake (devourFlakePath)
import Effectful (Eff, IOE, (:>))
import Effectful.Process (Process)
import Lucid (HtmlT, class_, div_, span_, toHtml)
import System.Nix.Core (nix)
import System.Nix.Version (NixVersion (..), getVersion)
import Vira.Environment.Tool.Type.ToolData (ToolData (..))

-- | Get Nix tool data with metadata and runtime info
getToolData :: (Process :> es, IOE :> es) => Eff es (ToolData (Either Text NixVersion))
getToolData = do
  version <- getVersion
  pure
    ToolData
      { name = "Nix"
      , url = "https://nixos.org"
      , binPaths = toText nix :| [toText devourFlakePath]
      , status = version
      }

-- | Display Nix version status
viewToolStatus :: (Monad m) => Either Text NixVersion -> HtmlT m ()
viewToolStatus = \case
  Left err ->
    div_ [class_ "text-sm"] $
      span_ [class_ "text-red-600 dark:text-red-400"] $
        "Error: " <> toHtml err
  Right (NixVersion ver) ->
    div_ [class_ "text-sm"] $
      span_ [class_ "text-gray-700 dark:text-gray-300"] $
        "Version: " <> toHtml ver
