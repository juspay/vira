{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Binary cache page HTTP handlers and views
module Vira.Web.Pages.CachePage (
  Routes (..),
  handlers,
) where

import Effectful.Reader.Dynamic (asks)
import Lucid
import NeatInterpolation (trimming)
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import System.Nix.Cache.Keys (PublicKey)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, runAppHtml)
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Alert qualified as Alert
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
  cachePublicKey <- lift $ asks @App.ViraRuntimeState (.cachePublicKey)
  Layout.layout [LinkTo.Cache] (viewCache cachePublicKey)

viewCache :: PublicKey -> AppHtml ()
viewCache cachePublicKey = do
  Layout.viraSection_ [] $ do
    Layout.viraPageHeaderWithIcon_ (toHtmlRaw Icon.database) "Binary Cache" $ do
      p_ [class_ "text-gray-600 dark:text-gray-300"] "Vira exposes the local Nix store as a binary cache server. Use this to get CI binaries on your local machine."

    Alert.viraAlert_ Alert.AlertWarning $ do
      p_ [class_ "text-yellow-800 dark:text-yellow-200"] $ do
        strong_ [class_ "font-semibold"] "Warning: "
        "This cache server is provided for lightweight use only. For production use, use an external cache like Attic or Cachix."

    W.viraCard_ [class_ "p-6"] $ do
      div_ [class_ "space-y-4"] $ do
        -- Cache URL section
        div_ [] $ do
          h3_ [class_ "text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2"] "Cache URL"
          Code.viraCodeBlockCopyable (Just "cache-url") cacheUrl
          p_ [class_ "text-sm text-gray-500 dark:text-gray-500 mt-2"] $ do
            "Check cache info: "
            a_ [href_ "/cache/nix-cache-info", class_ "text-indigo-600 dark:text-indigo-400 hover:underline", target_ "_blank"] "/cache/nix-cache-info"

        -- Public key section
        div_ [] $ do
          h3_ [class_ "text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2"] "Public Key"
          Code.viraCodeBlockCopyable Nothing cacheKey

        -- Usage instructions
        div_ [] $ do
          h3_ [class_ "text-sm font-semibold text-gray-700 dark:text-gray-300 mb-3"] "Usage"

          -- nix.conf subsection
          div_ [class_ "mb-4"] $ do
            h4_ [class_ "text-xs font-semibold text-gray-600 dark:text-gray-400 mb-2"] "In nix.conf"
            Code.viraCodeBlockCopyable
              (Just "nix-conf-example")
              [trimming|
              substituters = ${cacheUrl}
              trusted-public-keys = ${cacheKey}
            |]

          -- flake.nix subsection
          div_ [class_ "mb-4"] $ do
            h4_ [class_ "text-xs font-semibold text-gray-600 dark:text-gray-400 mb-2"] "In flake.nix (nixConfig)"
            Code.viraCodeBlockCopyable
              (Just "flake-nix-example")
              [trimming|
              nixConfig = {
                extra-substituters = [ "${cacheUrl}" ];
                extra-trusted-public-keys = [ "${cacheKey}" ];
              };
            |]

          -- CLI subsection
          div_ [] $ do
            h4_ [class_ "text-xs font-semibold text-gray-600 dark:text-gray-400 mb-2"] "CLI (--option flags)"
            Code.viraCodeBlockCopyable
              (Just "cli-example")
              [trimming|
              nix build \
                --option extra-substituters "${cacheUrl}" \
                --option extra-trusted-public-keys "${cacheKey}"
            |]

      -- Single script to replace VIRA-DOMAIN placeholder
      script_
        [trimming|
        document.addEventListener('DOMContentLoaded', () => {
          const origin = window.location.origin;
          ['cache-url', 'nix-conf-example', 'flake-nix-example', 'cli-example'].forEach(id => {
            const el = document.getElementById(id);
            if (el) el.textContent = el.textContent.replaceAll('VIRA-DOMAIN', origin);
          });
        });
      |]
  where
    cacheUrl = "VIRA-DOMAIN/cache"
    cacheKey = toText cachePublicKey
