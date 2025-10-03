{-# LANGUAGE OverloadedRecordDot #-}

-- | GitHub CLI tool information for ToolsPage
module Vira.Page.ToolsPage.GitHub (
  GhToolInfo (..),
  tool,
  display,
  view,
  read,
) where

import Data.Text qualified as T
import GH.Auth.Status (AuthStatus (..))
import GH.Auth.Status qualified as GH
import GH.Core qualified as GH
import GH.Signoff qualified as GH
import Lucid
import Vira.Page.ToolsPage.Core (Tool (..), ToolDisplay (..))
import Vira.Widgets.Alert qualified as W

newtype GhToolInfo = GhToolInfo
  { authStatus :: AuthStatus
  }

-- | Read GitHub CLI tool information
read :: IO GhToolInfo
read = GhToolInfo <$> GH.checkAuthStatus

-- | GitHub CLI tool definition
tool :: Tool
tool =
  Tool
    { name = "GitHub CLI"
    , description = "GitHub command line tool for various operations"
    , url = "https://cli.github.com"
    , binPaths = toText GH.ghBin :| [toText GH.ghSignoffBin]
    }

-- | GitHub CLI tool display styling
display :: ToolDisplay
display =
  ToolDisplay
    { initial = "G"
    , bgClass = "bg-green-100"
    , textClass = "text-green-600"
    }

-- | Render GitHub CLI tool information
view :: (Monad m) => GhToolInfo -> HtmlT m ()
view info = do
  div_ [class_ "mb-3"] $ do
    case info.authStatus of
      Authenticated {host, login, scopes} -> do
        W.viraAlert_ W.AlertSuccess $ do
          p_ [class_ "text-green-800 font-semibold mb-1"] $ do
            "✓ Authenticated as "
            strong_ $ toHtml login
            " on "
            strong_ $ toHtml host
          p_ [class_ "text-green-700 text-xs"] $ do
            "Scopes: "
            toHtml $ T.intercalate ", " scopes
      NotAuthenticated -> do
        W.viraAlert_ W.AlertError $ do
          p_ [class_ "text-red-800 mb-1"] "✗ Not authenticated"
          p_ [class_ "text-red-700 text-sm"] $ do
            "Run "
            code_ [class_ "bg-red-100 px-1 rounded"] "gh auth login"
            " to authenticate."
