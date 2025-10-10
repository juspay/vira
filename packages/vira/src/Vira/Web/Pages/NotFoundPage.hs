{- |
Custom 404 page for Vira.

This module provides a clean, branded 404 error page following the Vira Design System guidelines.
-}
module Vira.Web.Pages.NotFoundPage (
  complete404Page,
) where

import Data.Text.Lazy qualified as TL
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Dynamic qualified as Reader
import Lucid
import Vira.App.CLI (GlobalSettings (..), WebSettings (..))
import Vira.App.Stack (runApp)
import Vira.App.Type (ViraRuntimeState)
import Vira.Web.Lucid (runAppHtml)
import Vira.Web.Widgets.Layout qualified as W

{- |
Simple 404 page view following KISS principles.

Features:
- Clear error message
- Simple "Return to Home Page" link
- Consistent with Vira design system
-}
notFoundView :: (Monad m) => HtmlT m ()
notFoundView = do
  W.viraSection_ [class_ "text-center py-16"] $ do
    -- Simple error message with red background
    div_ [class_ "bg-red-50 border-2 border-red-200 rounded-xl p-8 mb-8"] $ do
      h1_ [class_ "text-6xl font-bold text-red-900 mb-4"] "404"
      h2_ [class_ "text-2xl font-semibold text-red-800 mb-4"] "Page Not Found"
      p_ [class_ "text-red-700 text-lg mb-8"] $ do
        "The page you're looking for doesn't exist."

    -- Simple return home link
    a_ [href_ "/", class_ "text-red-600 hover:text-red-800 underline"] "Return home"

{- |
Complete 404 page with full HTML structure using Vira's layout system.

This creates a standalone HTML page that includes all CSS and uses the
proper Vira layout for consistency with the rest of the application.
-}
complete404Page :: GlobalSettings -> ViraRuntimeState -> WebSettings -> IO TL.Text
complete404Page globalSettings viraRuntimeState webSettings = do
  result <- runApp globalSettings viraRuntimeState $ do
    Reader.runReader webSettings $ do
      runErrorNoCallStack $ do
        runAppHtml $ do
          W.layout mempty notFoundView
  case result of
    Left _err -> pure "Error generating 404 page"
    Right html -> pure $ renderText html
