{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tool definitions and operations for ToolsPage
module Vira.Page.ToolsPage.Tool (
  Tool (..),
  ToolMeta (..),
  ToolDisplay (..),
  toolMeta,
  toolDisplay,
  readInfo,
  renderInfo,
  allTools,
  readAllTools,
) where

import Attic qualified
import Attic.Config (AtticConfig (..), AtticServerConfig (..))
import Attic.Config qualified
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful.Git qualified as Git
import GH.Auth.Status (AuthStatus (..))
import GH.Auth.Status qualified as GH
import GH.Core qualified as GH
import GH.Signoff qualified as GH
import Lucid
import TOML (TOMLError)
import Vira.Lib.Cachix qualified as Cachix
import Vira.Lib.Omnix qualified as Omnix
import Vira.Widgets.Alert qualified as W

-- | Tool metadata
data ToolMeta = ToolMeta
  { name :: Text
  , description :: Text
  , url :: Text
  , binPaths :: NonEmpty Text
  }

-- | Tool display styling
data ToolDisplay = ToolDisplay
  { initial :: Text
  , bgClass :: Text
  , textClass :: Text
  }

-- | GADT for tool keys with their info types inlined
data Tool info where
  Attic :: Tool (Either TOMLError (Maybe AtticConfig))
  GitHub :: Tool AuthStatus
  Omnix :: Tool ()
  Git :: Tool ()
  Cachix :: Tool ()

$(deriveGEq ''Tool)
$(deriveGCompare ''Tool)

-- | Get metadata for a tool
toolMeta :: Tool info -> ToolMeta
toolMeta = \case
  Attic ->
    ToolMeta
      { name = "Attic"
      , description = "Self-hosted Nix binary cache server"
      , url = "https://github.com/zhaofengli/attic"
      , binPaths = toText Attic.atticBin :| []
      }
  GitHub ->
    ToolMeta
      { name = "GitHub CLI"
      , description = "GitHub command line tool for various operations"
      , url = "https://cli.github.com"
      , binPaths = toText GH.ghBin :| [toText GH.ghSignoffBin]
      }
  Omnix ->
    ToolMeta
      { name = "Omnix"
      , description = "A tool for building all Nix flake outputs"
      , url = "https://github.com/juspay/omnix"
      , binPaths = toText Omnix.omnixBin :| []
      }
  Git ->
    ToolMeta
      { name = "Git"
      , description = "Distributed version control system"
      , url = "https://git-scm.com"
      , binPaths = toText Git.git :| []
      }
  Cachix ->
    ToolMeta
      { name = "Cachix"
      , description = "Proprietary Nix binary cache hosting service"
      , url = "https://cachix.org"
      , binPaths = toText Cachix.cachixBin :| []
      }

-- | Get display styling for a tool
toolDisplay :: Tool info -> ToolDisplay
toolDisplay = \case
  Attic -> ToolDisplay {initial = "A", bgClass = "bg-indigo-100", textClass = "text-indigo-600"}
  GitHub -> ToolDisplay {initial = "G", bgClass = "bg-green-100", textClass = "text-green-600"}
  Omnix -> ToolDisplay {initial = "O", bgClass = "bg-purple-100", textClass = "text-purple-600"}
  Git -> ToolDisplay {initial = "G", bgClass = "bg-orange-100", textClass = "text-orange-600"}
  Cachix -> ToolDisplay {initial = "C", bgClass = "bg-blue-100", textClass = "text-blue-600"}

-- | Read runtime information for a tool
readInfo :: Tool info -> IO info
readInfo = \case
  Attic -> Attic.Config.readAtticConfig
  GitHub -> GH.checkAuthStatus
  Omnix -> pass
  Git -> pass
  Cachix -> pass

-- | Render tool-specific information
renderInfo :: (Monad m) => Tool info -> info -> HtmlT m ()
renderInfo Attic cfg = do
  div_ [class_ "mb-3"] $ do
    case cfg of
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
      Right (Just atticCfg) -> do
        W.viraAlert_ W.AlertSuccess $ do
          case atticCfg.defaultServer of
            Just defServer -> do
              p_ [class_ "text-green-800 font-semibold mb-1"] $ do
                "✓ Default server: "
                strong_ $ toHtml defServer
            Nothing -> do
              p_ [class_ "text-green-800 font-semibold mb-1"] "✓ Configured"

          -- Display all configured servers
          unless (Map.null atticCfg.servers) $ do
            p_ [class_ "text-green-700 text-xs mt-2 mb-1"] "Configured servers:"
            div_ [class_ "space-y-1"] $ do
              forM_ (Map.toList atticCfg.servers) $ \(serverName, serverCfg) -> do
                div_ [class_ "text-green-700 text-xs pl-2"] $ do
                  strong_ $ toHtml serverName
                  ": "
                  code_ [class_ "bg-green-100 px-1 rounded"] $ toHtml serverCfg.endpoint
                  case serverCfg.token of
                    Just _ -> span_ [class_ "ml-2 text-green-600"] "🔑"
                    Nothing -> span_ [class_ "ml-2 text-yellow-600"] "⚠ No token"
renderInfo GitHub status = do
  div_ [class_ "mb-3"] $ do
    case status of
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
renderInfo Omnix () = mempty
renderInfo Git () = mempty
renderInfo Cachix () = mempty

-- | All tools to display (in desired order)
allTools :: [DSum Tool (Const ())]
allTools =
  [ Omnix :=> Const ()
  , Git :=> Const ()
  , Attic :=> Const ()
  , Cachix :=> Const ()
  , GitHub :=> Const ()
  ]

-- | Read runtime information for all tools into a DMap
readAllTools :: IO (DMap Tool Identity)
readAllTools = fmap DMap.fromList $ forM allTools $ \(tool :=> _) -> do
  info <- readInfo tool
  pure (tool :=> Identity info)
