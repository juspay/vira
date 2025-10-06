-- | GitHub tool-specific logic
module Vira.Tool.Tools.GitHub (
  getToolData,
  viewToolStatus,
) where

import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import GH.Auth.Status (AuthStatus (..))
import GH.Auth.Status qualified as GH
import GH.Core qualified as GH
import GH.Signoff qualified as GH
import Lucid (HtmlT, class_, code_, div_, p_, strong_, toHtml)
import Vira.Tool.Type.ToolData (ToolData (..))
import Vira.Widgets.Alert (AlertType (..), viraAlert_)

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
      , status = info
      }

-- | View GitHub tool status
viewToolStatus :: (Monad m) => AuthStatus -> HtmlT m ()
viewToolStatus status = do
  div_ [class_ "mb-3"] $ do
    case status of
      Authenticated {host, login, scopes} -> do
        viraAlert_ AlertSuccess $ do
          p_ [class_ "text-green-800 font-semibold mb-1"] $ do
            "✓ Authenticated as "
            strong_ $ toHtml login
            " on "
            strong_ $ toHtml host
          p_ [class_ "text-green-700 text-xs"] $ do
            "Scopes: "
            toHtml $ T.intercalate ", " scopes
      NotAuthenticated -> do
        viraAlert_ AlertError $ do
          p_ [class_ "text-red-800 mb-1"] "✗ Not authenticated"
          p_ [class_ "text-red-700 text-sm"] $ do
            "Run "
            code_ [class_ "bg-red-100 px-1 rounded"] "gh auth login"
            " to authenticate."
