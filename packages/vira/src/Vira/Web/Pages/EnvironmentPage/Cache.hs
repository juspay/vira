{-# LANGUAGE OverloadedRecordDot #-}

-- | Cache section view for Environment page
module Vira.Web.Pages.EnvironmentPage.Cache (
  viewCache,
) where

import Lucid
import System.Directory (doesFileExist)
import Vira.App.CLI (GlobalSettings (..))
import Vira.Cache.Keys (readPublicKey)
import Vira.Web.Lucid (AppHtml)
import Vira.Web.Widgets.Card qualified as W
import Vira.Web.Widgets.Code qualified as Code
import Web.TablerIcons.Outline qualified as Icon

viewCache :: GlobalSettings -> AppHtml ()
viewCache globalSettings = do
  let publicKeyPath = globalSettings.stateDir <> "/cache-keys/public-key"
  publicKeyExists <- liftIO $ doesFileExist publicKeyPath

  when publicKeyExists $ do
    publicKey <- liftIO $ readPublicKey publicKeyPath
    viewCacheCard publicKey

-- | View the cache server card
viewCacheCard :: (Monad m) => Text -> HtmlT m ()
viewCacheCard publicKey = do
  h2_ [class_ "text-xl font-semibold text-gray-900 dark:text-gray-100 mb-4 flex items-center mt-8"] $ do
    div_ [class_ "w-5 h-5 mr-2 flex items-center justify-center"] $ toHtmlRaw Icon.database
    "Binary Cache"
  p_ [class_ "text-gray-600 dark:text-gray-300 mb-4"] "Nix binary cache server serving local builds"

  W.viraCard_ [class_ "p-6"] $ do
    div_ [class_ "space-y-4"] $ do
      -- Cache URL section
      div_ [] $ do
        h3_ [class_ "text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2"] "Cache URL"
        code_ [id_ "cache-url", class_ "block px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded font-mono"] ""
        -- Client-side script to construct URL
        script_ $
          unlines
            [ "document.addEventListener('DOMContentLoaded', function() {"
            , "  const cacheUrl = window.location.origin + '/cache';"
            , "  document.getElementById('cache-url').textContent = cacheUrl;"
            , "});"
            ]

      -- Public key section
      div_ [] $ do
        h3_ [class_ "text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2"] "Public Key"
        Code.viraCodeCopyable_ publicKey

      -- Usage instructions
      div_ [] $ do
        h3_ [class_ "text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2"] "Usage"
        p_ [class_ "text-sm text-gray-600 dark:text-gray-400 mb-2"] "Add to your nix.conf or use with --option:"

        code_ [id_ "nix-conf-example", class_ "block px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded font-mono whitespace-pre"] ""
        -- Client-side script to construct config example
        script_ $
          unlines
            [ "document.addEventListener('DOMContentLoaded', function() {"
            , "  const cacheUrl = window.location.origin + '/cache';"
            , "  const publicKey = '" <> publicKey <> "';"
            , "  const example = 'substituters = ' + cacheUrl + '\\n' + 'trusted-public-keys = ' + publicKey;"
            , "  document.getElementById('nix-conf-example').textContent = example;"
            , "});"
            ]

        p_ [class_ "text-sm text-gray-500 dark:text-gray-500 mt-2"] $ do
          "Or use "
          Code.viraCodeInline_ "--option"
          " flags when running nix commands"
