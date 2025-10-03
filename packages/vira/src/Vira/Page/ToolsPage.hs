-- | Tools information page
module Vira.Page.ToolsPage (
  Routes (..),
  handlers,
)
where

import Attic qualified
import GH.Signoff qualified as GH
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App (AppHtml)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.Lib.Cachix qualified as Cachix
import Vira.Lib.Omnix qualified as Omnix
import Vira.Widgets.Card qualified as W
import Vira.Widgets.Layout qualified as W
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (for_)

newtype Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.AppState -> WebSettings -> Routes AsServer
handlers cfg webSettings =
  Routes
    { _view = App.runAppInServant cfg webSettings . App.runAppHtml $ viewHandler
    }

viewHandler :: AppHtml ()
viewHandler = do
  W.layout [LinkTo.Tools] viewTools

viewTools :: AppHtml ()
viewTools = do
  W.viraSection_ [] $ do
    W.viraPageHeader_ "Tools" $ do
      p_ [class_ "text-gray-600"] "Command-line tools used by Vira for CI/CD operations"

    div_ [class_ "grid gap-6 md:grid-cols-2 lg:grid-cols-2"] $ do
      -- Omnix
      toolCard
        "O"
        "bg-purple-100"
        "text-purple-600"
        "Omnix"
        "Nix CI/CD orchestration tool for running builds"
        "https://github.com/juspay/omnix"
        (toText Omnix.omnixBin)

      -- Attic
      toolCard
        "A"
        "bg-indigo-100"
        "text-indigo-600"
        "Attic"
        "Self-hosted Nix binary cache server"
        "https://github.com/zhaofengli/attic"
        (toText Attic.atticBin)

      -- Cachix
      toolCard
        "C"
        "bg-blue-100"
        "text-blue-600"
        "Cachix"
        "Hosted Nix binary cache service"
        "https://cachix.org"
        (toText Cachix.cachixBin)

      -- GitHub CLI (gh-signoff)
      toolCard
        "G"
        "bg-green-100"
        "text-green-600"
        "GitHub CLI"
        "Git signoff automation via gh-signoff extension"
        "https://cli.github.com"
        (toText GH.ghSignoffBin)

toolCard :: (Monad m) => Text -> Text -> Text -> Text -> Text -> Text -> Text -> HtmlT m ()
toolCard initial bgClass textClass name description url binPath = do
  W.viraCard_ [class_ "p-6"] $ do
    div_ [class_ "flex items-start mb-4"] $ do
      span_ [class_ $ "h-12 w-12 mr-4 " <> bgClass <> " rounded-lg flex items-center justify-center " <> textClass <> " font-bold text-xl"] $
        toHtml initial
      div_ [class_ "flex-1"] $ do
        h3_ [class_ "text-xl font-bold text-gray-900 mb-2"] $ toHtml name
        p_ [class_ "text-gray-600 text-sm mb-3"] $ toHtml description
        div_ [class_ "mb-3"] $ do
          code_ [class_ "text-xs bg-gray-100 text-gray-700 px-2 py-1 rounded font-mono"] $ toHtml binPath
        a_
          [ href_ url
          , target_ "_blank"
          , class_ "inline-flex items-center gap-1 text-indigo-600 hover:text-indigo-800 text-sm font-medium"
          ]
          $ do
            span_ "Learn more"
            span_ [class_ "w-4 h-4 flex items-center"] $ toHtmlRaw Icon.external_link
