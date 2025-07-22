{-# LANGUAGE OverloadedRecordDot #-}

-- | Common Lucid rendering helpers
module Vira.Widgets (
  layout,
  viraButton_,
  viraInput_,
  viraLabel_,
) where

import Lucid
import Servant.Links (Link, URI (..), linkURI)
import Vira.App (AppState (cliSettings, linkTo), instanceName)
import Vira.App.CLI (CLISettings (basePath))
import Vira.App.LinkTo.Type (LinkTo, linkShortTitle)
import Vira.Lib.HTMX
import Vira.Stream.Status qualified as Status

-- | Common HTML layout for all routes.
layout :: AppState -> [LinkTo] -> Html () -> Html ()
layout cfg crumbs content = do
  doctype_
  html_ $ do
    head_ $ do
      mobileFriendly
      title_ $ do
        case viaNonEmpty last crumbs of
          Nothing -> mempty
          Just link -> do
            toHtml $ linkShortTitle link
            " - "
        toHtml siteTitle
      base_ [href_ cfg.cliSettings.basePath]
      -- favicon
      link_ [rel_ "icon", type_ "image/jpg", href_ "vira-logo.jpg"]
      htmx
      link_ [rel_ "stylesheet", type_ "text/css", href_ "tailwind.css"]
    body_ [class_ "bg-orange-50"] $ do
      div_ [class_ "container mx-auto p-4 mt-8 bg-white rounded shadow-lg"] $ do
        let crumbs' = crumbs <&> \l -> (toHtml $ linkShortTitle l, linkURI $ cfg.linkTo l)
        breadcrumbs cfg.linkTo crumbs'
        content
  where
    siteTitle = "Vira (" <> cfg.cliSettings.instanceName <> ")"
    -- Mobile friendly head tags
    mobileFriendly = do
      meta_ [charset_ "utf-8", name_ "viewport", content_ "width=device-width, initial-scale=1"]
    -- JavaScript include for HTMX
    htmx = do
      script_ [src_ "js/htmx.min.js"] $ mempty @Text
      script_ [src_ "js/hyperscript.min.js"] $ mempty @Text
      script_ [src_ "js/htmx-ext-debug.js"] $ mempty @Text
      -- We use a fork of htmx-ext-sse
      -- See https://github.com/bigskysoftware/htmx-extensions/pull/147
      -- script_ [src_ "https://unpkg.com/htmx-ext-sse@2.2.2"] $ mempty @Text
      script_ [src_ "js/htmx-extensions/src/sse/sse.js"] $ mempty @Text

-- | Show breadcrumbs at the top of the page for navigation to parent routes
breadcrumbs :: (LinkTo -> Link) -> [(Html (), URI)] -> Html ()
breadcrumbs linkTo rs' = do
  let home = URI {uriScheme = "", uriAuthority = Nothing, uriPath = "", uriQuery = [], uriFragment = ""}
      logo = img_ [src_ "vira-logo.jpg", alt_ "Vira Logo", style_ "height: 32px;"]
      rs = (logo, home) :| rs'
  nav_ [id_ "breadcrumbs", class_ "flex items-center text-sm p-2 mb-4 bg-orange-700 rounded"] $ do
    ol_ [class_ "flex flex-1 items-center space-x-1 text-xl list-none"] $
      renderCrumbs (toList rs)
    Status.viewStream linkTo
  where
    renderCrumbs = \case
      [] -> pass
      [x] -> do
        li_ [class_ "flex items-center"] $ renderCrumb (fst x, Nothing)
      (x : xs) -> do
        li_ [class_ "flex items-center"] $ renderCrumb (second Just x)
        li_ [class_ "flex items-center"] chevronSvg
        renderCrumbs xs
    renderCrumb :: (Html (), Maybe URI) -> Html ()
    renderCrumb (s, mr) = case mr of
      Just r ->
        a_
          [ href_ (show r)
          , class_ "text-gray-100 hover:text-white transition-colors px-2 py-1 rounded focus:outline-none focus:ring-2 focus:ring-white"
          ]
          $ toHtml s
      Nothing ->
        span_ [class_ "font-bold text-white px-2 py-1 rounded bg-orange-800"] $ toHtml s
    chevronSvg =
      span_ [class_ "mx-1 text-gray-300"] $ toHtmlRaw ("<svg xmlns='http://www.w3.org/2000/svg' class='h-4 w-4' fill='none' viewBox='0 0 24 24' stroke='currentColor' stroke-width='2'><path stroke-linecap='round' stroke-linejoin='round' d='M9 5l7 7-7 7'/></svg>" :: Text)

viraButton_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraButton_ attrs =
  button_
    ( [ class_ "inline-flex h-12 items-center justify-center rounded-md bg-blue-950 px-4 my-2 font-medium text-neutral-50 shadow-lg shadow-blue-500/20 transition active:scale-95"
      , type_ "button"
      , hyperscript_ "on click toggle .bg-green-500 until htmx:afterOnLoad"
      ]
        <> attrs
    )

viraInput_ :: forall (m :: Type -> Type). (Monad m) => [Attributes] -> HtmlT m ()
viraInput_ attrs = do
  input_ ([class_ "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500"] <> attrs)

viraLabel_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraLabel_ attrs = do
  label_ ([class_ "block text-sm font-medium text-gray-700"] <> attrs)
