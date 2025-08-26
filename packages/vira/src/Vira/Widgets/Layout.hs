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
  viraSection_,
  viraDivider_,
) where

import Lucid
import Servant.Links (Link, URI (..), linkURI)
import Vira.App.CLI (CLISettings (basePath), instanceName)
import Vira.App.LinkTo.Type (LinkTo, linkShortTitle)
import Vira.App.Stack (AppState (cliSettings, linkTo))
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
          [ "html { overflow-y: scroll; }" -- Scrollbar always visible, to prevent jankiness
          , "body { font-family: 'Inter', ui-sans-serif, system-ui, sans-serif; }"
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

{- |
Standardized page header with title and subtitle.

Creates consistent page headers across the application with:
- Large, bold title typography
- Subtle subtitle area for descriptions
- Bottom border for visual separation
- Proper spacing and hierarchy

= Usage Examples

@
-- Basic page header
W.viraPageHeader_ "Settings" $ do
  p_ [class_ "text-gray-600"] "Configure your CI/CD settings"

-- Header with multiple subtitle elements
W.viraPageHeader_ "Repository Details" $ do
  div_ [class_ "flex items-center space-x-4 text-gray-600"] $ do
    span_ "Last updated: 2 hours ago"
    span_ [class_ "px-2 py-1 bg-green-100 text-green-800 text-xs rounded-full"] "Active"

-- Simple header without subtitle
W.viraPageHeader_ "Build Logs" mempty
@

= Typography Guidelines

Title uses text-3xl font-bold for strong hierarchy.
Subtitle area should use text-gray-600 for proper contrast.
-}
viraPageHeader_ :: Text -> Html () -> Html ()
viraPageHeader_ title subtitle = do
  div_ [class_ "border-b border-gray-200 pb-6 mb-8"] $ do
    h1_ [class_ "text-3xl font-bold text-gray-900 tracking-tight"] $ toHtml title
    div_ [class_ "mt-2 text-gray-600"] subtitle

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
viraDivider_ :: Html ()
viraDivider_ = do
  hr_ [class_ "border-gray-200 my-6"]
