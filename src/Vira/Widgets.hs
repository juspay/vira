{-# LANGUAGE OverloadedRecordDot #-}

-- | Common Lucid rendering helpers
module Vira.Widgets (
  layout,
  viraButton_,
) where

import Lucid
import Servant.Links (Link, URI (..), linkURI)
import Vira.App (AppState (linkTo, settings), instanceName)
import Vira.App.CLI (Settings (basePath))
import Vira.App.LinkTo.Type (LinkTo, linkShortTitle)
import Vira.Lib.HTMX
import Vira.Status qualified as Status

-- | Common HTML layout for all routes.
layout :: AppState -> Html () -> [LinkTo] -> Html () -> Html ()
layout cfg heading crumbs content = do
  doctype_
  html_ $ do
    head_ $ do
      mobileFriendly
      title_ $ toHtml @Text $ "Vira (" <> cfg.settings.instanceName <> ")"
      base_ [href_ cfg.settings.basePath]
      htmx
      link_ [rel_ "stylesheet", type_ "text/css", href_ "tailwind.css"]
    body_ [class_ "bg-gray-100"] $ do
      div_ [class_ "container mx-auto p-4 mt-8 bg-white"] $ do
        let crumbs' = crumbs <&> \l -> (toHtml $ linkShortTitle l, linkURI $ cfg.linkTo l)
        breadcrumbs cfg.linkTo crumbs'
        h1_ [class_ "text-3xl border-b-2 mb-2"] heading
        content
  where
    -- Mobile friendly head tags
    mobileFriendly = do
      meta_ [charset_ "utf-8", name_ "viewport", content_ "width=device-width, initial-scale=1"]
    -- JavaScript include for HTMX
    htmx = do
      script_ [src_ "https://unpkg.com/htmx.org@2.0.3", integrity_ "sha384-0895/pl2MU10Hqc6jd4RvrthNlDiE9U1tWmX7WRESftEDRosgxNsQG/Ze9YMRzHq", crossorigin_ "anonymous"] $ mempty @Text
      script_ [src_ "https://unpkg.com/hyperscript.org@0.9.13"] $ mempty @Text
      script_ [src_ "https://unpkg.com/htmx-ext-debug@2.0.0/debug.js"] $ mempty @Text
      -- We use a fork of htmx-ext-sse
      -- See https://github.com/bigskysoftware/htmx-extensions/pull/147
      -- script_ [src_ "https://unpkg.com/htmx-ext-sse@2.2.2"] $ mempty @Text
      script_ [src_ "htmx-extensions/src/sse/sse.js"] $ mempty @Text

-- | Show breadcrumbs at the top of the page for navigation to parent routes
breadcrumbs :: (LinkTo -> Link) -> [(Html (), URI)] -> Html ()
breadcrumbs linkTo rs' = do
  let home = URI {uriScheme = "", uriAuthority = Nothing, uriPath = "/", uriQuery = [], uriFragment = ""}
  let rs = (span_ "Vira", home) :| rs'
  nav_ [id_ "breadcrumbs", class_ "flex items-center space-x-2 text-sm text-gray-600 p-3 mb-4 bg-blue-100"] $ do
    div_ [class_ "flex flex-1 items-center space-x-2 text-gray-600"] $ do
      forM_ (init rs) $ \(s, r) -> do
        renderCrumb (s, Just r)
        span_ [class_ "text-gray-500"] ">"
      renderCrumb (fst $ last rs, Nothing)
    Status.view linkTo
  where
    renderCrumb :: (Html (), Maybe URI) -> Html ()
    renderCrumb (s, mr) = li_ [class_ "flex"] $ do
      let attr = case mr of
            Just r ->
              let url = if show @Text r == "" then "/" else show r
               in [href_ url, class_ "hover:underline"]
            Nothing -> [class_ "font-bold"]
      a_ attr $ toHtml s

viraButton_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraButton_ attrs =
  button_
    ( [ class_ "inline-flex h-12 items-center justify-center rounded-md bg-blue-950 px-4 my-2 font-medium text-neutral-50 shadow-lg shadow-blue-500/20 transition active:scale-95"
      , type_ "button"
      , hyperscript_ "on click toggle .bg-green-500 until htmx:afterOnLoad"
      ]
        <> attrs
    )
