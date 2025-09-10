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
  appLogoUrl,
  viraPageHeader_,
  viraPageHeaderWithIcon_,
  viraSection_,
  viraDivider_,
) where

import Effectful.Reader.Dynamic (asks)
import Lucid
import Vira.App qualified as App
import Vira.App.CLI (WebSettings (..))
import Vira.App.InstanceInfo (InstanceInfo (..), platform)
import Vira.App.LinkTo.Type (LinkTo (..), linkShortTitle, linkTitle)
import Vira.App.Lucid (AppHtml)
import Vira.App.Stack (AppState)
import Vira.Stream.Refresh qualified as Refresh
import Vira.Widgets.Status qualified as Status
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (asks)

-- | Generate page title with emoji and hostname suffix
pageTitle :: InstanceInfo -> Maybe Text -> Text
pageTitle instanceInfo = \case
  Nothing -> siteTitle
  Just title -> title <> " - " <> siteTitle
  where
    siteTitle = "Vira[" <> instanceInfo.hostname <> "]"

-- | Dynamic favicon based on platform
appLogoUrl :: AppHtml Text
appLogoUrl = do
  instanceInfo <- lift $ asks @AppState (.instanceInfo)
  case instanceInfo.os of
    "linux" -> pure "vira-logo-penguin.svg"
    "macos" -> pure "vira-logo-apple.svg"
    _ -> pure "vira-logo.svg"

-- | Common HTML layout for all routes.
layout :: [LinkTo] -> AppHtml () -> AppHtml ()
layout crumbs content = do
  instanceInfo <- lift $ asks @AppState (.instanceInfo)
  logoUrl <- appLogoUrl
  basePath <- lift $ asks @WebSettings (.basePath)
  doctype_
  html_ $ do
    head_ $ do
      mobileFriendly
      title_ $ do
        let baseTitle = linkTitle <$> viaNonEmpty last crumbs
        toHtml $ pageTitle instanceInfo baseTitle
      base_ [href_ basePath]
      -- Google Fonts - Inter for modern, clean typography
      link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
      link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ ""]
      link_ [href_ "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap", rel_ "stylesheet"]
      link_ [rel_ "icon", type_ "image/svg+xml", href_ logoUrl]
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
      Refresh.viewStream
      div_ [class_ "min-h-screen flex flex-col"] $ do
        -- Main container with clean styling
        div_ [class_ "container mx-auto px-4 py-3 lg:px-8 flex-1"] $ do
          breadcrumbs crumbs
          content
        footer crumbs
  where
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
    -- Footer with memory usage information
    footer :: [LinkTo] -> AppHtml ()
    footer _crumbs = do
      instanceInfo <- lift $ asks @AppState (.instanceInfo)
      div_ [class_ "bg-gray-100 border-t border-gray-200 mt-auto"] $ do
        div_ [class_ "container mx-auto px-4 py-3 lg:px-8"] $ do
          div_ [class_ "flex justify-between items-center text-sm text-gray-600"] $ do
            div_ [class_ "flex items-center space-x-4"] $ do
              span_ [title_ "Hostname", class_ "cursor-help"] $ toHtml instanceInfo.hostname
              span_ [class_ "text-gray-400"] "•"
              span_ [title_ "Platform", class_ "cursor-help"] $ toHtml (platform instanceInfo)
            div_ [class_ "text-xs text-gray-500"] $ do
              a_ [href_ "https://github.com/juspay/vira", target_ "_blank", class_ "hover:text-gray-700 transition-colors"] "Vira"

-- | Get icon for a LinkTo type
linkToIcon :: (Monad m) => LinkTo -> HtmlT m ()
linkToIcon = \case
  Home -> toHtmlRaw Icon.home
  RepoListing -> toHtmlRaw Icon.folder
  Repo _ -> toHtmlRaw Icon.book_2
  RepoBranch _ _ -> toHtmlRaw Icon.git_branch
  Job _ -> toHtmlRaw Icon.player_play
  Settings -> toHtmlRaw Icon.settings_2
  _ -> toHtmlRaw Icon.circle -- fallback for other types

-- | Show breadcrumbs at the top of the page for navigation to parent routes
breadcrumbs :: [LinkTo] -> AppHtml ()
breadcrumbs rs' = do
  logoUrl <- appLogoUrl
  let logo = img_ [src_ logoUrl, alt_ "Vira Logo", class_ "h-8 w-8 rounded-lg"]
  nav_ [id_ "breadcrumbs", class_ "flex items-center justify-between px-4 py-2 bg-indigo-600 rounded-t-xl"] $ do
    ol_ [class_ "flex flex-1 items-center space-x-2 text-base list-none"] $ do
      -- Logo as first element
      li_ [class_ "flex items-center"] $ do
        basePath <- lift $ asks @WebSettings (.basePath)
        a_ [href_ basePath, class_ "font-semibold text-white px-2 py-1 rounded-lg hover:bg-white/30 transition-smooth"] logo
      -- Render breadcrumb links
      renderCrumbs rs'
    Status.viewAllJobStatus
  where
    renderCrumbs :: [LinkTo] -> AppHtml ()
    renderCrumbs = \case
      [] -> pass
      [x] -> do
        li_ [class_ "flex items-center"] chevronSvg
        li_ [class_ "flex items-center"] $ renderCrumb x True
      (x : xs) -> do
        li_ [class_ "flex items-center"] chevronSvg
        li_ [class_ "flex items-center"] $ renderCrumb x False
        renderCrumbs xs
    renderCrumb :: LinkTo -> Bool -> AppHtml ()
    renderCrumb linkToValue isLast = do
      let title = linkShortTitle linkToValue
          icon = linkToIcon linkToValue
          content = do
            div_ [class_ "w-4 h-4 mr-2 flex items-center justify-center"] icon
            toHtml title
      if isLast
        then span_ [class_ "font-semibold text-white px-2 py-1 rounded-lg bg-white/20 backdrop-blur-sm flex items-center"] content
        else do
          url <- lift $ App.getLinkUrl linkToValue
          a_
            [ href_ url
            , class_ "text-white/90 hover:text-white transition-smooth px-2 py-1 rounded-lg hover:bg-white/10 focus:outline-none focus:ring-2 focus:ring-white/30 font-medium flex items-center"
            ]
            content
    chevronSvg :: (Monad m) => HtmlT m ()
    chevronSvg =
      span_ [class_ "mx-1 text-white/60"] $ toHtmlRaw ("<svg xmlns='http://www.w3.org/2000/svg' class='h-5 w-5' fill='none' viewBox='0 0 24 24' stroke='currentColor' stroke-width='2'><path stroke-linecap='round' stroke-linejoin='round' d='M9 5l7 7-7 7'/></svg>" :: Text)

{- |
Standardized page header with title and subtitle.

Features indigo styling that connects visually with breadcrumbs.
-}
viraPageHeader_ :: (Monad m) => Text -> HtmlT m () -> HtmlT m ()
viraPageHeader_ title subtitle = do
  div_ [class_ "bg-indigo-50 border-2 border-t-0 border-indigo-200 rounded-b-xl p-4 mb-6"] $ do
    h1_ [class_ "text-2xl font-bold text-indigo-900 tracking-tight mb-2"] $ toHtml title
    div_ [class_ "text-indigo-700"] subtitle

{- |
Standardized page header with icon, title and subtitle.

Features indigo styling that connects visually with breadcrumbs, with an icon displayed alongside the title.
-}
viraPageHeaderWithIcon_ :: (Monad m) => HtmlT m () -> Text -> HtmlT m () -> HtmlT m ()
viraPageHeaderWithIcon_ icon title subtitle = do
  div_ [class_ "bg-indigo-50 border-2 border-t-0 border-indigo-200 rounded-b-xl p-4 mb-6"] $ do
    h1_ [class_ "text-2xl font-bold text-indigo-900 tracking-tight mb-2 flex items-center"] $ do
      div_ [class_ "w-6 h-6 mr-3 flex items-center justify-center text-indigo-900"] icon
      toHtml title
    div_ [class_ "text-indigo-700"] subtitle

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
