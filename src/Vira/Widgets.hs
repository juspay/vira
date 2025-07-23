{-# LANGUAGE OverloadedRecordDot #-}

{- |
Vira Design System - Reusable UI Components

This module contains all reusable UI components for the Vira CI/CD application.
All components follow the Vira Design System guidelines defined in DESIGN.md.

= Component Categories

== Layout Components
- 'viraSection_' - Page section wrapper with consistent spacing
- 'viraCard_' - Card container with elegant shadows and rounded corners
- 'viraPageHeader_' - Standardized page headers with title and subtitle
- 'viraDivider_' - Visual content separator

== Interactive Components
- 'viraButton_' - Primary action buttons with hover states
- 'viraIconButton_' - Icon-only buttons for secondary actions
- 'viraInput_' - Form input fields with proper focus states
- 'viraLabel_' - Form labels with consistent typography

== Display Components
- 'viraStatusBadge_' - Status indicators with semantic colors
- 'viraCodeBlock_' - Code display with proper formatting
- 'viraCodeInline_' - Inline code elements
- 'viraAlert_' - Alert messages (success, error, warning, info)
- 'viraFormGroup_' - Form field grouping for consistent layouts

= Usage Guidelines

Always use these components instead of raw HTML to maintain design consistency.
Follow the Vira Design System color palette and spacing guidelines.

= Design Principles

- Modern & Professional: Clean, contemporary design
- Clarity & Focus: Clear information hierarchy
- Consistency: Unified visual language
- Accessibility: Inclusive design with proper focus states
-}
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

{- |
Enhanced button component with improved styling and hover states.

This is the primary button component for all user actions. It includes:
- Smooth transitions and micro-interactions
- Proper focus states for accessibility
- Disabled state handling
- Consistent indigo brand colors by default

= Usage Examples

@
-- Primary action (most important action on page)
W.viraButton_ [class_ "bg-indigo-600 hover:bg-indigo-700 focus:ring-indigo-500"] "Save Changes"

-- Success action
W.viraButton_ [class_ "bg-green-600 hover:bg-green-700 focus:ring-green-500"] "✅ Build"

-- Destructive action
W.viraButton_ [class_ "bg-red-600 hover:bg-red-700 focus:ring-red-500"] "🗑️ Delete"

-- Submit button in forms
W.viraButton_ [type_ "submit", form_ "my-form"] "Submit"
@

= Styling Guidelines

Override colors using Tailwind classes while maintaining the component structure.
Always include hover and focus ring colors that match the background color.
-}
viraButton_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraButton_ attrs =
  button_
    ( [ class_ "inline-flex items-center justify-center px-6 py-3 text-sm font-semibold rounded-lg transition-smooth focus:outline-none focus:ring-2 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed bg-indigo-600 hover:bg-indigo-700 text-white shadow-md hover:shadow-lg focus:ring-indigo-500"
      , hyperscript_ "on click add .scale-95 then wait 100ms then remove .scale-95"
      ]
        <> attrs
    )

{- |
Icon button variant for secondary actions and toolbar buttons.

Smaller, more subtle button for icon-only actions. Perfect for:
- Toolbar actions
- Settings buttons
- Secondary controls that don't need emphasis

= Usage Examples

@
-- Settings action
W.viraIconButton_ [] "⚙️"

-- Edit action
W.viraIconButton_ [title_ "Edit"] "✏️"

-- Close/cancel action
W.viraIconButton_ [onclick_ "closeModal()"] "✕"
@

= Design Notes

Uses neutral colors by default to avoid competing with primary actions.
Always include a title attribute for accessibility when using icons.
-}
viraIconButton_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraIconButton_ attrs =
  button_
    ( [ class_ "inline-flex items-center justify-center p-2 text-sm font-medium rounded-lg transition-smooth focus:outline-none focus:ring-2 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed bg-white hover:bg-gray-50 text-gray-700 border border-gray-300 shadow-sm hover:shadow-md focus:ring-indigo-500"
      ]
        <> attrs
    )

{- |
Card container component with elegant styling and hover effects.

The primary container for grouping related content. Features:
- Elegant shadow that lifts on hover
- Rounded corners and subtle borders
- Consistent spacing and overflow handling
- Glass-morphism inspired design

= Usage Examples

@
-- Basic content card
W.viraCard_ [class_ "p-6"] $ do
  h3_ [class_ "text-lg font-semibold mb-4"] "Card Title"
  p_ [class_ "text-gray-600"] "Card content goes here"

-- Repository card with custom padding
W.viraCard_ [class_ "p-4 hover:bg-gray-50"] $ do
  -- Repository details

-- Settings section card
W.viraCard_ [class_ "p-6 mb-6"] $ do
  -- Configuration form
@

= Layout Guidelines

Use consistent padding: p-4 for compact cards, p-6 for standard cards.
Combine with grid layouts for responsive card grids.
-}
viraCard_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraCard_ attrs =
  div_
    ( [ class_ "bg-white rounded-xl border border-gray-200 shadow-elegant hover:shadow-lg transition-smooth overflow-hidden"
      ]
        <> attrs
    )

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
    W.viraStatusBadge_ "Active" "bg-green-100 text-green-800"

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
Status badge component with semantic color variants.

Displays status information with appropriate colors and styling.
Follows the Vira Design System status color guidelines.

= Usage Examples

@
-- Success status
W.viraStatusBadge_ "Success" "bg-green-100 text-green-800 border-green-200"

-- Error status
W.viraStatusBadge_ "Failed" "bg-red-100 text-red-800 border-red-200"

-- Warning/Pending status
W.viraStatusBadge_ "Pending" "bg-yellow-100 text-yellow-800 border-yellow-200"

-- Info/Running status
W.viraStatusBadge_ "Running" "bg-blue-100 text-blue-800 border-blue-200"

-- Killed status (darker red for distinction)
W.viraStatusBadge_ "Killed" "bg-red-200 text-red-900 border-red-300"
@

= Color Guidelines

Use semantic colors that match the status meaning:
- Green: Success, connected, healthy states
- Red: Errors, failures, killed processes
- Yellow: Warnings, pending states
- Blue: Information, running processes
- Gray: Neutral, inactive states
-}
viraStatusBadge_ :: Text -> Text -> Html ()
viraStatusBadge_ status colorClass = do
  span_ [class_ $ "inline-flex items-center px-3 py-1 rounded-full text-sm font-medium " <> colorClass] $
    toHtml status

{- |
Code block component for displaying larger code snippets.

Formatted container for multi-line code with:
- Monospace font family
- Subtle background and borders
- Horizontal scrolling for overflow
- Proper text selection and copying

= Usage Examples

@
-- Git commit hash
W.viraCodeBlock_ "a1b2c3d4e5f6g7h8i9j0"

-- Build command
W.viraCodeBlock_ "nix build .#default"

-- Error message
W.viraCodeBlock_ "Error: Package not found in registry"
@

= When to Use

Use for longer code snippets, commit hashes, commands, or error messages.
For inline code within text, use 'viraCodeInline_' instead.
-}
viraCodeBlock_ :: Text -> Html ()
viraCodeBlock_ code = do
  div_ [class_ "bg-gray-50 border border-gray-200 rounded-lg p-4 overflow-x-auto"] $ do
    code_ [class_ "text-sm text-gray-800 font-mono break-all"] $ toHtml code

{- |
Inline code component for small code snippets within text.

Compact styling for short code elements that appear inline with text.
Perfect for commit hashes, variable names, and short commands.

= Usage Examples

@
-- Commit hash in job listing
div_ $ do
  "Commit: "
  W.viraCodeInline_ "a1b2c3d4"

-- Variable name in documentation
p_ $ do
  "Set the "
  W.viraCodeInline_ "API_KEY"
  " environment variable"

-- Short command
span_ $ do
  "Run "
  W.viraCodeInline_ "just build"
@

= Design Guidelines

Uses smaller, more subtle styling than 'viraCodeBlock_'.
Integrates seamlessly with surrounding text flow.
-}
viraCodeInline_ :: Text -> Html ()
viraCodeInline_ code = do
  code_ [class_ "px-2 py-1 text-xs bg-gray-100 text-gray-700 rounded font-mono"] $ toHtml code

{- |
Alert component for displaying important messages with semantic colors.

Provides user feedback with appropriate visual styling and icons.
Includes accessibility features with proper ARIA roles.

= Usage Examples

@
-- Success message
W.viraAlert_ "success" "bg-green-50 border-green-200" $ do
  p_ [class_ "text-green-800"] "Repository successfully added!"

-- Error message
W.viraAlert_ "error" "bg-red-50 border-red-200" $ do
  p_ [class_ "text-red-800"] "Failed to connect to repository"

-- Warning message
W.viraAlert_ "warning" "bg-yellow-50 border-yellow-200" $ do
  p_ [class_ "text-yellow-800"] "This action cannot be undone"

-- Info message
W.viraAlert_ "info" "bg-blue-50 border-blue-200" $ do
  p_ [class_ "text-blue-800"] "Configure Cachix to enable build caching"
@

= Icons

Automatically includes appropriate emoji icons:
- ✅ for success alerts
- ❌ for error alerts
- ⚠️ for warning alerts
- ℹ️ for info alerts

= Accessibility

Includes role="alert" for screen readers.
-}
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

{- |
Form group component for consistent form field layout.

Groups labels and inputs with proper spacing for professional forms.
Ensures consistent spacing and alignment across all form fields.

= Usage Examples

@
-- Basic form field
W.viraFormGroup_
  (W.viraLabel_ [for_ "username"] "Username")
  (W.viraInput_ [type_ "text", name_ "username", id_ "username"])

-- With validation state
W.viraFormGroup_
  (W.viraLabel_ [for_ "email"] "Email Address")
  (div_ $ do
    W.viraInput_ [type_ "email", name_ "email", id_ "email"]
    W.viraAlert_ "error" "bg-red-50" $ do
      p_ [class_ "text-red-600 text-sm"] "Please enter a valid email")
@

= Layout Guidelines

Use this for all form fields to maintain consistent spacing.
Pairs perfectly with 'viraLabel_' and 'viraInput_' components.
-}
viraFormGroup_ :: Html () -> Html () -> Html ()
viraFormGroup_ label input = do
  div_ [class_ "space-y-2"] $ do
    label
    input

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

-- Form related widgets below

{- |
Form input component with consistent styling and focus states.

Provides standardized input fields across the application with:
- Consistent padding, borders, and border radius
- Proper focus states with indigo accent colors
- Background and transition styling
- Accessibility features

= Usage Examples

@
-- Basic text input
W.viraInput_ [type_ "text", name_ "username", placeholder_ "Enter username"]

-- Email input with validation
W.viraInput_ [type_ "email", name_ "email", required_ "", placeholder_ "user@example.com"]

-- Password input
W.viraInput_ [type_ "password", name_ "password", placeholder_ "Enter password"]

-- With initial value
W.viraInput_ [type_ "text", name_ "repo", value_ existingValue]
@

= Styling Guidelines

Inherits full width (w-full) by default.
Override styling with additional classes as needed.
Focus ring uses indigo-500 to match brand colors.
-}
viraInput_ :: forall (m :: Type -> Type). (Monad m) => [Attributes] -> HtmlT m ()
viraInput_ attrs = do
  input_ ([class_ "block w-full px-4 py-3 text-sm border border-gray-300 rounded-lg shadow-sm placeholder-gray-500 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 bg-white transition-colors duration-200"] <> attrs)

{- |
Form label component with consistent typography and spacing.

Provides standardized labels for form fields with:
- Consistent font weight and size
- Proper text color for readability
- Bottom margin for spacing from input

= Usage Examples

@
-- Basic label
W.viraLabel_ [for_ "username"] "Username"

-- Required field indicator
W.viraLabel_ [for_ "email"] $ do
  "Email Address"
  span_ [class_ "text-red-500"] " *"

-- With help text
div_ $ do
  W.viraLabel_ [for_ "api-key"] "API Key"
  p_ [class_ "text-xs text-gray-500"] "Found in your account settings"
@

= Accessibility Guidelines

Always include for_ attribute that matches the input's id.
This ensures proper label-input association for screen readers.
-}
viraLabel_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraLabel_ attrs = do
  label_ ([class_ "block text-sm font-semibold text-gray-700 mb-1"] <> attrs)
