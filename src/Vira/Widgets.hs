{-# LANGUAGE OverloadedRecordDot #-}

-- | Common Lucid rendering helpers
module Vira.Widgets (
  layout,
  viraButton_,
  viraInput_,
  viraLabel_,
  viraCard_,
  viraSection_,
  viraPageHeader_,
  viraStatusBadge_,
  viraCodeBlock_,
  viraCodeInline_,
  viraAlert_,
  viraFormGroup_,
  viraIconButton_,
  viraDivider_,
) where

import Lucid
import Servant.Links (Link, URI (..), linkURI)
import Vira.App.CLI (CLISettings (basePath), instanceName)
import Vira.App.LinkTo.Type (LinkTo, linkShortTitle)
import Vira.App.Stack (AppState (cliSettings, linkTo))
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
      -- Google Fonts - Inter for modern, clean typography
      link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
      link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ ""]
      link_ [href_ "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap", rel_ "stylesheet"]
      -- favicon
      link_ [rel_ "icon", type_ "image/jpg", href_ "vira-logo.jpg"]
      htmx
      link_ [rel_ "stylesheet", type_ "text/css", href_ "tailwind.css"]
      -- Custom styles for the new design
      style_ $
        unlines
          [ "body { font-family: 'Inter', ui-sans-serif, system-ui, sans-serif; }"
          , ".gradient-bg { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); }"
          , ".glass-effect { backdrop-filter: blur(10px); background: rgba(255, 255, 255, 0.1); }"
          , ".shadow-elegant { box-shadow: 0 10px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04); }"
          , ".transition-smooth { transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); }"
          ]
    body_ [class_ "bg-gradient-to-br from-slate-50 to-blue-50 min-h-screen font-inter"] $ do
      div_ [class_ "min-h-screen"] $ do
        -- Main container with improved styling
        div_ [class_ "container mx-auto px-4 py-6 lg:px-8"] $ do
          div_ [class_ "bg-white/80 backdrop-blur-sm border border-white/20 rounded-2xl shadow-elegant p-6 lg:p-8"] $ do
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
      logo = img_ [src_ "vira-logo.jpg", alt_ "Vira Logo", class_ "h-8 w-8 rounded-lg shadow-sm"]
      rs = (logo, home) :| rs'
  nav_ [id_ "breadcrumbs", class_ "flex items-center justify-between p-4 mb-6 bg-gradient-to-r from-indigo-600 via-purple-600 to-blue-600 rounded-xl shadow-lg"] $ do
    ol_ [class_ "flex flex-1 items-center space-x-2 text-lg list-none"] $
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
          , class_ "text-white/90 hover:text-white transition-smooth px-3 py-2 rounded-lg hover:bg-white/10 focus:outline-none focus:ring-2 focus:ring-white/30 font-medium"
          ]
          $ toHtml s
      Nothing ->
        span_ [class_ "font-semibold text-white px-3 py-2 rounded-lg bg-white/20 backdrop-blur-sm"] $ toHtml s
    chevronSvg =
      span_ [class_ "mx-1 text-white/60"] $ toHtmlRaw ("<svg xmlns='http://www.w3.org/2000/svg' class='h-5 w-5' fill='none' viewBox='0 0 24 24' stroke='currentColor' stroke-width='2'><path stroke-linecap='round' stroke-linejoin='round' d='M9 5l7 7-7 7'/></svg>" :: Text)

-- | Enhanced button component with improved styling and variants
viraButton_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraButton_ attrs =
  button_
    ( [ class_ "inline-flex items-center justify-center px-6 py-3 text-sm font-semibold rounded-lg transition-smooth focus:outline-none focus:ring-2 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed bg-indigo-600 hover:bg-indigo-700 text-white shadow-md hover:shadow-lg focus:ring-indigo-500"
      , type_ "button"
      , hyperscript_ "on click add .scale-95 then wait 100ms then remove .scale-95"
      ]
        <> attrs
    )

-- | Icon button variant for actions
viraIconButton_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraIconButton_ attrs =
  button_
    ( [ class_ "inline-flex items-center justify-center p-2 text-sm font-medium rounded-lg transition-smooth focus:outline-none focus:ring-2 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed bg-white hover:bg-gray-50 text-gray-700 border border-gray-300 shadow-sm hover:shadow-md focus:ring-indigo-500"
      , type_ "button"
      ]
        <> attrs
    )

-- | Card container component
viraCard_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraCard_ attrs =
  div_
    ( [ class_ "bg-white rounded-xl border border-gray-200 shadow-elegant hover:shadow-lg transition-smooth overflow-hidden"
      ]
        <> attrs
    )

-- | Section component for grouping content
viraSection_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraSection_ attrs =
  section_
    ( [ class_ "space-y-6"
      ]
        <> attrs
    )

-- | Page header component
viraPageHeader_ :: Text -> Html () -> Html ()
viraPageHeader_ title subtitle = do
  div_ [class_ "border-b border-gray-200 pb-6 mb-8"] $ do
    h1_ [class_ "text-3xl font-bold text-gray-900 tracking-tight"] $ toHtml title
    div_ [class_ "mt-2 text-gray-600"] subtitle

-- | Status badge component with color variants
viraStatusBadge_ :: Text -> Text -> Html ()
viraStatusBadge_ status colorClass = do
  span_ [class_ $ "inline-flex items-center px-3 py-1 rounded-full text-sm font-medium " <> colorClass] $
    toHtml status

-- | Code block component
viraCodeBlock_ :: Text -> Html ()
viraCodeBlock_ code = do
  div_ [class_ "bg-gray-50 border border-gray-200 rounded-lg p-4 overflow-x-auto"] $ do
    code_ [class_ "text-sm text-gray-800 font-mono break-all"] $ toHtml code

-- | Inline code component for smaller code snippets
viraCodeInline_ :: Text -> Html ()
viraCodeInline_ code = do
  code_ [class_ "px-2 py-1 text-xs bg-gray-100 text-gray-700 rounded font-mono"] $ toHtml code

-- | Alert component with variants
viraAlert_ :: Text -> Text -> Html () -> Html ()
viraAlert_ alertType colorClass content = do
  div_ [class_ $ "rounded-lg p-4 border " <> colorClass, role_ "alert"] $ do
    div_ [class_ "flex items-start"] $ do
      div_ [class_ "flex-shrink-0"] $ do
        case alertType of
          "error" -> span_ [class_ "text-red-500"] "❌"
          "warning" -> span_ [class_ "text-yellow-500"] "⚠️"
          "success" -> span_ [class_ "text-green-500"] "✅"
          "info" -> span_ [class_ "text-blue-500"] "ℹ️"
          _ -> span_ [class_ "text-gray-500"] "•"
      div_ [class_ "ml-3 flex-1"] content

-- | Form group component for consistent form layout
viraFormGroup_ :: Html () -> Html () -> Html ()
viraFormGroup_ label input = do
  div_ [class_ "space-y-2"] $ do
    label
    input

-- | Divider component
viraDivider_ :: Html ()
viraDivider_ = do
  hr_ [class_ "border-gray-200 my-6"]

-- Form related widgets below

viraInput_ :: forall (m :: Type -> Type). (Monad m) => [Attributes] -> HtmlT m ()
viraInput_ attrs = do
  input_ ([class_ "block w-full px-4 py-3 text-sm border border-gray-300 rounded-lg shadow-sm placeholder-gray-500 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 transition-smooth"] <> attrs)

viraLabel_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraLabel_ attrs = do
  label_ ([class_ "block text-sm font-semibold text-gray-700 mb-1"] <> attrs)
