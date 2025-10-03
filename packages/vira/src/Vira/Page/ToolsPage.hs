{-# LANGUAGE OverloadedRecordDot #-}

-- | Tools information page
module Vira.Page.ToolsPage (
  Routes (..),
  handlers,
)
where

import Attic qualified
import Attic.Config (AtticConfig (..), AtticServerConfig (..))
import Attic.Config qualified
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful.Git qualified as Git
import GH.Auth.Status (AuthStatus (..))
import GH.Auth.Status qualified as GH
import GH.Core qualified as GH
import GH.Signoff qualified as GH
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import TOML (TOMLError)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Cachix qualified as Cachix
import Vira.Lib.Omnix qualified as Omnix
import Vira.Widgets.Alert qualified as W
import Vira.Widgets.Card qualified as W
import Vira.Widgets.Layout qualified as W
import Web.TablerIcons.Outline qualified as Icon

newtype Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

data Tool = Tool
  { name :: Text
  , description :: Text
  , url :: Text
  , binPaths :: NonEmpty Text
  }

newtype GhToolInfo = GhToolInfo
  { authStatus :: AuthStatus
  }

newtype AtticToolInfo = AtticToolInfo
  { config :: Either TOMLError (Maybe AtticConfig)
  }

handlers :: App.AppState -> WebSettings -> Routes AsServer
handlers cfg webSettings =
  Routes
    { _view = App.runAppInServant cfg webSettings . App.runAppHtml $ viewHandler
    }

viewHandler :: AppHtml ()
viewHandler = do
  ghAuthStatus <- lift $ liftIO GH.checkAuthStatus
  atticConfig <- lift $ liftIO Attic.Config.readAtticConfig
  W.layout [LinkTo.Tools] (viewTools ghAuthStatus atticConfig)

viewTools :: AuthStatus -> Either TOMLError (Maybe AtticConfig) -> AppHtml ()
viewTools ghAuthStatus atticConfig = do
  W.viraSection_ [] $ do
    W.viraPageHeader_ "Tools" $ do
      p_ [class_ "text-gray-600"] "Command-line tools used by Vira jobs"

    div_ [class_ "grid gap-6 md:grid-cols-2 lg:grid-cols-2"] $ do
      toolCard "O" "bg-purple-100" "text-purple-600" omnix mempty
      toolCard "G" "bg-orange-100" "text-orange-600" gitTool mempty
      toolCard "A" "bg-indigo-100" "text-indigo-600" attic (atticToolInfo $ AtticToolInfo atticConfig)
      toolCard "C" "bg-blue-100" "text-blue-600" cachix mempty
      toolCard "G" "bg-green-100" "text-green-600" githubCli (ghToolInfo $ GhToolInfo ghAuthStatus)
  where
    omnix =
      Tool
        { name = "Omnix"
        , description = "A tool for building all Nix flake outputs"
        , url = "https://github.com/juspay/omnix"
        , binPaths = toText Omnix.omnixBin :| []
        }
    gitTool =
      Tool
        { name = "Git"
        , description = "Distributed version control system"
        , url = "https://git-scm.com"
        , binPaths = toText Git.git :| []
        }
    attic =
      Tool
        { name = "Attic"
        , description = "Self-hosted Nix binary cache server"
        , url = "https://github.com/zhaofengli/attic"
        , binPaths = toText Attic.atticBin :| []
        }
    cachix =
      Tool
        { name = "Cachix"
        , description = "Proprietary Nix binary cache hosting service"
        , url = "https://cachix.org"
        , binPaths = toText Cachix.cachixBin :| []
        }
    githubCli =
      Tool
        { name = "GitHub CLI"
        , description = "GitHub command line tool for various operations"
        , url = "https://cli.github.com"
        , binPaths = toText GH.ghBin :| [toText GH.ghSignoffBin]
        }

toolCard :: (Monad m) => Text -> Text -> Text -> Tool -> HtmlT m () -> HtmlT m ()
toolCard initial bgClass textClass tool extraInfo = do
  W.viraCard_ [class_ "p-6"] $ do
    div_ [class_ "flex items-start mb-4"] $ do
      span_ [class_ $ "h-12 w-12 mr-4 " <> bgClass <> " rounded-lg flex items-center justify-center " <> textClass <> " font-bold text-xl"] $
        toHtml initial
      div_ [class_ "flex-1"] $ do
        h3_ [class_ "text-xl font-bold text-gray-900 mb-2"] $ toHtml tool.name
        p_ [class_ "text-gray-600 text-sm mb-3"] $ toHtml tool.description
        div_ [class_ "mb-3 space-y-1"] $ do
          forM_ tool.binPaths $ \binPath ->
            code_ [class_ "block text-xs bg-gray-100 text-gray-700 px-2 py-1 rounded font-mono"] $ toHtml binPath

        -- Extra info (e.g., auth status)
        extraInfo

        a_
          [ href_ tool.url
          , target_ "_blank"
          , class_ "inline-flex items-center gap-1 text-indigo-600 hover:text-indigo-800 text-sm font-medium"
          ]
          $ do
            span_ "Learn more"
            span_ [class_ "w-4 h-4 flex items-center"] $ toHtmlRaw Icon.external_link

ghToolInfo :: (Monad m) => GhToolInfo -> HtmlT m ()
ghToolInfo info = do
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

atticToolInfo :: (Monad m) => AtticToolInfo -> HtmlT m ()
atticToolInfo info = do
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
