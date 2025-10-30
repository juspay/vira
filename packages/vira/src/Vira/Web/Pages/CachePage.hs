{-# LANGUAGE OverloadedRecordDot #-}

-- | Binary cache page HTTP handlers and views
module Vira.Web.Pages.CachePage (
  Routes (..),
  handlers,
) where

import Effectful.Reader.Dynamic (asks)
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.Cache.Server (CacheInfo (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, runAppHtml)
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Card qualified as W
import Vira.Web.Widgets.Code qualified as Code
import Vira.Web.Widgets.Layout qualified as Layout
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (asks)

newtype Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings =
  Routes
    { _view = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml $ viewHandler
    }

viewHandler :: AppHtml ()
viewHandler = do
  cacheInfo <- lift $ asks @App.ViraRuntimeState (.cacheInfo)
  Layout.layout [LinkTo.Cache] (viewCache cacheInfo)

viewCache :: CacheInfo -> AppHtml ()
viewCache cacheInfo = do
  Layout.viraSection_ [] $ do
    Layout.viraPageHeaderWithIcon_ (toHtmlRaw Icon.database) "Binary Cache" $ do
      p_ [class_ "text-gray-600 dark:text-gray-300"] "Nix binary cache server serving local builds"

    W.viraCard_ [class_ "p-6"] $ do
      div_ [class_ "space-y-4"] $ do
        -- Cache URL section
        div_ [] $ do
          h3_ [class_ "text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2"] "Cache URL"
          Code.viraCodeBlockCopyableJs "cache-url"
          -- Client-side script to construct URL
          script_ $
            unlines
              [ "document.addEventListener('DOMContentLoaded', function() {"
              , "  const cacheUrl = window.location.origin + '/cache';"
              , "  document.getElementById('cache-url').textContent = cacheUrl;"
              , "});"
              ]
          p_ [class_ "text-sm text-gray-500 dark:text-gray-500 mt-2"] $ do
            "Check cache info: "
            a_ [href_ "/cache/nix-cache-info", class_ "text-indigo-600 dark:text-indigo-400 hover:underline", target_ "_blank"] "/cache/nix-cache-info"

        -- Public key section
        div_ [] $ do
          h3_ [class_ "text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2"] "Public Key"
          Code.viraCodeBlockCopyable cacheInfo.publicKey

        -- Usage instructions
        div_ [] $ do
          h3_ [class_ "text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2"] "Usage"
          p_ [class_ "text-sm text-gray-600 dark:text-gray-400 mb-2"] "Add to your nix.conf or use with --option:"

          Code.viraCodeBlockCopyableJs "nix-conf-example"
          -- Client-side script to construct config example
          script_ $
            unlines
              [ "document.addEventListener('DOMContentLoaded', function() {"
              , "  const cacheUrl = window.location.origin + '/cache';"
              , "  const publicKey = '" <> cacheInfo.publicKey <> "';"
              , "  const example = 'substituters = ' + cacheUrl + '\\n' + 'trusted-public-keys = ' + publicKey;"
              , "  document.getElementById('nix-conf-example').textContent = example;"
              , "});"
              ]

          p_ [class_ "text-sm text-gray-500 dark:text-gray-500 mt-2"] $ do
            "Or use "
            code_ [class_ "px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded font-mono"] "--option"
            " flags when running nix commands"
