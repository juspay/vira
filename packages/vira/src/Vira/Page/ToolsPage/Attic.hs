{-# LANGUAGE OverloadedRecordDot #-}

-- | Attic tool information for ToolsPage
module Vira.Page.ToolsPage.Attic (
  AtticToolInfo (..),
  tool,
  display,
  view,
  read,
) where

import Attic qualified
import Attic.Config (AtticConfig (..), AtticServerConfig (..))
import Attic.Config qualified
import Data.Map.Strict qualified as Map
import Lucid
import TOML (TOMLError)
import Vira.Page.ToolsPage.Core (Tool (..), ToolDisplay (..))
import Vira.Widgets.Alert qualified as W

newtype AtticToolInfo = AtticToolInfo
  { config :: Either TOMLError (Maybe AtticConfig)
  }

-- | Read Attic tool information
read :: IO AtticToolInfo
read = AtticToolInfo <$> Attic.Config.readAtticConfig

-- | Attic tool definition
tool :: Tool
tool =
  Tool
    { name = "Attic"
    , description = "Self-hosted Nix binary cache server"
    , url = "https://github.com/zhaofengli/attic"
    , binPaths = toText Attic.atticBin :| []
    }

-- | Attic tool display styling
display :: ToolDisplay
display =
  ToolDisplay
    { initial = "A"
    , bgClass = "bg-indigo-100"
    , textClass = "text-indigo-600"
    }

-- | Render Attic tool information
view :: (Monad m) => AtticToolInfo -> HtmlT m ()
view info = do
  div_ [class_ "mb-3"] $ do
    case info.config of
      Left err -> do
        W.viraAlert_ W.AlertError $ do
          p_ [class_ "text-red-800 font-semibold mb-1"] "✗ Parse error"
          p_ [class_ "text-red-700 text-sm"] $ toHtml (show err :: String)
      Right Nothing -> do
        W.viraAlert_ W.AlertWarning $ do
          p_ [class_ "text-yellow-800 mb-1"] "⚠ Not configured"
          p_ [class_ "text-yellow-700 text-sm"] $ do
            "Config file not found at "
            code_ [class_ "bg-yellow-100 px-1 rounded"] "~/.config/attic/config.toml"
      Right (Just cfg) -> do
        W.viraAlert_ W.AlertSuccess $ do
          case cfg.defaultServer of
            Just defServer -> do
              p_ [class_ "text-green-800 font-semibold mb-1"] $ do
                "✓ Default server: "
                strong_ $ toHtml defServer
            Nothing -> do
              p_ [class_ "text-green-800 font-semibold mb-1"] "✓ Configured"

          -- Display all configured servers
          unless (Map.null cfg.servers) $ do
            p_ [class_ "text-green-700 text-xs mt-2 mb-1"] "Configured servers:"
            div_ [class_ "space-y-1"] $ do
              forM_ (Map.toList cfg.servers) $ \(serverName, serverCfg) -> do
                div_ [class_ "text-green-700 text-xs pl-2"] $ do
                  strong_ $ toHtml serverName
                  ": "
                  code_ [class_ "bg-green-100 px-1 rounded"] $ toHtml serverCfg.endpoint
                  case serverCfg.token of
                    Just _ -> span_ [class_ "ml-2 text-green-600"] "🔑"
                    Nothing -> span_ [class_ "ml-2 text-yellow-600"] "⚠ No token"
