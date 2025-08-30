{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Vira Design System - Layout Components

This module contains layout infrastructure components for the Vira CI/CD application.
All components follow the Vira Design System guidelines defined in DESIGN.md.

= Component Categories

== Layout Infrastructure
- 'layout' - Common HTML layout wrapper for all routes
- 'breadcrumbs' - Navigation breadcrumbs with logo and status
- 'viraPageHeader_' - Standardized page headers with title and subtitle
- 'viraSection_' - Page section wrapper with consistent spacing
- 'viraDivider_' - Visual content separator

= Usage Guidelines

Use these components to create consistent page layouts and content structure.
The layout function provides the foundation for all application pages.

= Design Principles

- Structural Consistency: Unified page architecture
- Navigation Clarity: Clear breadcrumb navigation
- Visual Hierarchy: Proper content organization
- Responsive Design: Works across all device sizes
-}
module Vira.Widgets.Layout (
  layout,
  viraPageHeader_,
  viraPageHeaderWithActions_,
  viraSection_,
  viraDivider_,
) where

import Lucid
import Servant.Links (Link, URI (..), linkURI)
import Vira.App.CLI (CLISettings (basePath), instanceName)
import Vira.App.LinkTo.Type (LinkTo, linkShortTitle)
import Vira.App.Stack (AppState (cliSettings, linkTo), VHtml, hoistVHtml)
import Vira.Stream.Status qualified as Status

-- | Common HTML layout for all routes.
layout :: AppState -> [LinkTo] -> Html () -> VHtml ()
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
          [ "html { overflow-y: scroll; }" -- Scrollbar always visible, to prevent jankiness
          , "body { font-family: 'Inter', ui-sans-serif, system-ui, sans-serif; }"
          , ".transition-smooth { transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); }"
          ]
    body_ [class_ "bg-gray-50 min-h-screen font-inter"] $ do
      div_ [class_ "min-h-screen"] $ do
        -- Main container with clean styling
        div_ [class_ "container mx-auto px-4 py-6 lg:px-8"] $ do
          let crumbs' = crumbs <&> \l -> (toHtml $ linkShortTitle l, linkURI $ cfg.linkTo l)
          breadcrumbs cfg.linkTo crumbs'
          hoistVHtml content
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
breadcrumbs :: (LinkTo -> Link) -> [(Html (), URI)] -> VHtml ()
breadcrumbs linkTo rs' = do
  let home = URI {uriScheme = "", uriAuthority = Nothing, uriPath = "", uriQuery = [], uriFragment = ""}
      logo = img_ [src_ "vira-logo.jpg", alt_ "Vira Logo", class_ "h-8 w-8 rounded-lg"]
      rs = (logo, home) :| rs'
  nav_ [id_ "breadcrumbs", class_ "flex items-center justify-between p-4 bg-indigo-600 rounded-t-xl"] $ do
    hoistVHtml $
      ol_ [class_ "flex flex-1 items-center space-x-2 text-lg list-none"] $
        renderCrumbs (toList rs)
    Status.viewStream linkTo
  where
    renderCrumbs :: [(Html (), URI)] -> Html ()
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
          s
      Nothing ->
        span_ [class_ "font-semibold text-white px-3 py-2 rounded-lg bg-white/20 backdrop-blur-sm"] s
    chevronSvg =
      span_ [class_ "mx-1 text-white/60"] $ toHtmlRaw ("<svg xmlns='http://www.w3.org/2000/svg' class='h-5 w-5' fill='none' viewBox='0 0 24 24' stroke='currentColor' stroke-width='2'><path stroke-linecap='round' stroke-linejoin='round' d='M9 5l7 7-7 7'/></svg>" :: Text)

{- |
Standardized page header with title and subtitle.

Features indigo styling that connects visually with breadcrumbs.
-}
viraPageHeader_ :: (Monad m) => Text -> HtmlT m () -> HtmlT m ()
viraPageHeader_ title subtitle = do
  div_ [class_ "bg-indigo-50 border-2 border-t-0 border-indigo-200 rounded-b-xl p-8 mb-8"] $ do
    h1_ [class_ "text-4xl font-bold text-indigo-900 tracking-tight mb-4"] $ toHtml title
    div_ [class_ "text-indigo-700"] subtitle

{- |
Page header with title, subtitle, and action buttons.

Same styling as viraPageHeader_ but supports action buttons on the right.
-}
viraPageHeaderWithActions_ :: (Monad m) => Text -> HtmlT m () -> HtmlT m () -> HtmlT m ()
viraPageHeaderWithActions_ title subtitle actions = do
  div_ [class_ "bg-indigo-50 border-2 border-t-0 border-indigo-200 rounded-b-xl p-8 mb-8"] $ do
    div_ [class_ "flex items-start justify-between"] $ do
      div_ [class_ "flex-1"] $ do
        h1_ [class_ "text-4xl font-bold text-indigo-900 tracking-tight mb-4"] $ toHtml title
        div_ [class_ "text-indigo-700"] subtitle
      div_ [class_ "flex flex-col gap-3 ml-8"] actions

{- |
Section component for grouping and spacing page content.

Provides consistent vertical spacing between content sections.
Use this as the main wrapper for page content areas.

= Usage Examples

@
-- Main page content
W.viraSection_ [] $ do
  W.viraPageHeader_ "Page Title" $ do
    p_ [class_ "text-gray-600"] "Page description"

  W.viraCard_ [class_ "p-6"] $ do
    -- Main content

-- Custom spacing
W.viraSection_ [class_ "space-y-8"] $ do
  -- Content with larger spacing
@

= Spacing System

Default spacing is space-y-6 (24px). Override with space-y-* classes as needed.
-}
viraSection_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraSection_ attrs =
  section_
    ( [ class_ "space-y-6"
      ]
        <> attrs
    )

{- |
Visual divider component for separating content sections.

Creates a subtle horizontal line to separate content areas.
Includes consistent spacing above and below.

= Usage Examples

@
-- Between content sections
div_ $ do
  -- First section content
  W.viraDivider_
  -- Second section content

-- In cards between different areas
W.viraCard_ [class_ "p-6"] $ do
  h3_ "Connected Services"
  W.viraAlert_ "success" "..." $ do
    -- Success message
  W.viraDivider_
  -- Configuration form
@

= Spacing Guidelines

Includes my-6 (24px) vertical margin for proper content separation.
-}
viraDivider_ :: (Monad m) => HtmlT m ()
viraDivider_ = do
  hr_ [class_ "border-gray-200 my-6"]
