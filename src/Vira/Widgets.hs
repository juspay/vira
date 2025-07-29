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
  viraFilterInput_,

  -- * Type-safe enums
  AlertType (..),
  ButtonVariant (..),
) where

import Data.Char (toUpper)
import Data.Text (cons, isSuffixOf, splitOn)
import Data.Text qualified as T
import Lucid
import Servant.Links (Link, URI (..), linkURI)
import Vira.App.CLI (CLISettings (basePath), instanceName)
import Vira.App.LinkTo.Type (LinkTo, linkShortTitle)
import Vira.App.Stack (AppState (cliSettings, linkTo))
import Vira.Lib.HTMX (hyperscript_)
import Vira.State.Core qualified as St
import Vira.Stream.Status qualified as Status

-- | Alert types for consistent messaging
data AlertType
  = AlertSuccess
  | AlertError
  | AlertWarning
  | AlertInfo
  deriving stock (Eq, Show)

-- | Button variant types for consistent styling
data ButtonVariant
  = -- | Indigo - primary actions, forms
    ButtonPrimary
  | -- | Red - delete, disconnect, kill actions
    ButtonDestructive
  | -- | Green - build, success actions
    ButtonSuccess
  | -- | Gray - secondary actions
    ButtonSecondary
  deriving stock (Eq, Show)

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
Enhanced button component with type-safe styling variants.

This is the primary button component for all user actions. It includes:
- Type-safe variant system for consistent styling
- Smooth transitions and micro-interactions
- Proper focus states for accessibility
- Disabled state handling
- Automatic color coordination

= Usage Examples

@
-- Primary action (most important action on page)
W.viraButton_ W.ButtonPrimary [type_ "submit"] "Save Changes"

-- Success action
W.viraButton_ W.ButtonSuccess [] "✅ Build"

-- Destructive action
W.viraButton_ W.ButtonDestructive [] "🗑️ Delete"

-- Secondary action
W.viraButton_ W.ButtonSecondary [] "Cancel"

-- With additional attributes
W.viraButton_ W.ButtonPrimary [type_ "submit", form_ "my-form"] "Submit"
@

= Variant Guidelines

- **ButtonPrimary**: Main actions, form submissions (indigo)
- **ButtonSuccess**: Positive actions like build, save (green)
- **ButtonDestructive**: Delete, disconnect, kill actions (red)
- **ButtonSecondary**: Less important actions, cancel (gray)

= Type Safety

Colors are automatically managed by the variant type, preventing inconsistent styling.
-}
viraButton_ :: forall {result}. (Term [Attributes] result) => ButtonVariant -> [Attributes] -> result
viraButton_ variant attrs =
  let (colorClasses, focusRing) = case variant of
        ButtonPrimary -> ("bg-indigo-600 hover:bg-indigo-700 text-white", "focus:ring-indigo-500")
        ButtonSuccess -> ("bg-green-600 hover:bg-green-700 text-white", "focus:ring-green-500")
        ButtonDestructive -> ("bg-red-600 hover:bg-red-700 text-white", "focus:ring-red-500")
        ButtonSecondary -> ("bg-gray-600 hover:bg-gray-700 text-white", "focus:ring-gray-500")
   in button_
        ( [ class_ $ "inline-flex items-center justify-center px-6 py-3 text-sm font-semibold rounded-lg transition-smooth focus:outline-none focus:ring-2 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed shadow-md hover:shadow-lg " <> colorClasses <> " " <> focusRing
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
Status badge component with semantic color variants.

Displays status information with appropriate colors and styling.
Follows the Vira Design System status color guidelines.
Now uses the domain JobStatus type directly for type safety.

= Usage Examples

@
-- Job status badges
W.viraStatusBadge_ St.JobRunning
W.viraStatusBadge_ St.JobPending
W.viraStatusBadge_ (St.JobFinished St.JobSuccess)
W.viraStatusBadge_ (St.JobFinished St.JobFailure)
W.viraStatusBadge_ St.JobKilled
@

= Color Guidelines

Colors are automatically applied based on job status:
- Green: Successful jobs
- Red: Failed or killed jobs
- Yellow: Pending jobs
- Blue: Running jobs

= Type Safety

Uses 'St.JobStatus' directly from the domain model to prevent invalid status values.
-}
viraStatusBadge_ :: St.JobStatus -> Html ()
viraStatusBadge_ jobStatus = do
  let (statusText, colorClass) = case jobStatus of
        St.JobRunning -> ("Running" :: Text, "bg-blue-100 text-blue-800 border-blue-200")
        St.JobPending -> ("Pending" :: Text, "bg-yellow-100 text-yellow-800 border-yellow-200")
        St.JobFinished St.JobSuccess -> ("Success" :: Text, "bg-green-100 text-green-800 border-green-200")
        St.JobFinished St.JobFailure -> ("Failed" :: Text, "bg-red-100 text-red-800 border-red-200")
        St.JobKilled -> ("Killed" :: Text, "bg-red-200 text-red-900 border-red-300")
  span_ [class_ $ "inline-flex items-center px-3 py-1 rounded-full text-sm font-medium " <> colorClass] $
    toHtml statusText

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
Now type-safe with automatic color management based on alert type.

= Usage Examples

@
-- Success message
W.viraAlert_ W.AlertSuccess $ do
  p_ [class_ "text-green-800"] "Repository successfully added!"

-- Error message
W.viraAlert_ W.AlertError $ do
  p_ [class_ "text-red-800"] "Failed to connect to repository"

-- Warning message
W.viraAlert_ W.AlertWarning $ do
  p_ [class_ "text-yellow-800"] "This action cannot be undone"

-- Info message
W.viraAlert_ W.AlertInfo $ do
  p_ [class_ "text-blue-800"] "Configure Cachix to enable build caching"
@

= Color Management

Colors are automatically applied based on alert type:
- Green: Success messages (bg-green-50 border-green-200)
- Red: Error messages (bg-red-50 border-red-200)
- Yellow: Warning messages (bg-yellow-50 border-yellow-200)
- Blue: Info messages (bg-blue-50 border-blue-200)

= Icons

Automatically includes appropriate emoji icons:
- ✅ for success alerts
- ❌ for error alerts
- ⚠️ for warning alerts
- ℹ️ for info alerts

= Type Safety

Uses 'AlertType' ADT to prevent invalid alert types at compile time.
No manual color class management required.

= Accessibility

Includes role="alert" for screen readers.
-}
viraAlert_ :: AlertType -> Html () -> Html ()
viraAlert_ alertType content = do
  let (colorClass, iconText :: Text, iconColor) = case alertType of
        AlertError -> ("bg-red-50 border-red-200", "❌", "text-red-500")
        AlertWarning -> ("bg-yellow-50 border-yellow-200", "⚠️", "text-yellow-500")
        AlertSuccess -> ("bg-green-50 border-green-200", "✅", "text-green-500")
        AlertInfo -> ("bg-blue-50 border-blue-200", "ℹ️", "text-blue-500")
  div_ [class_ $ "rounded-lg p-4 border " <> colorClass, role_ "alert"] $ do
    div_ [class_ "flex items-start"] $ do
      div_ [class_ "flex-shrink-0"] $ do
        span_ [class_ iconColor] (toHtml iconText)
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

{- |
Filter input component for real-time content filtering.

Provides a search input with integrated filtering functionality using hyperscript.
Perfect for filtering large lists like branches, jobs, or repositories.
Includes search icon and follows design system styling.

= Usage Examples

@
-- Basic branch filtering
W.viraFilterInput_
  "[data-branch-item]"
  [placeholder_ "Filter branches..."]

-- Custom styling and attributes
W.viraFilterInput_
  "[data-repo-item]"
  [placeholder_ "Search repositories...", class_ "mb-4", id_ "repo-search"]

-- Job filtering with custom placeholder
W.viraFilterInput_
  "[data-job-item]"
  [placeholder_ "Find builds...", autofocus_ ""]
@

= Parameters

- **targetSelector**: CSS selector for elements to filter (e.g. "[data-branch-item]")
- **attrs**: Additional HTML attributes including placeholder, id, class, etc.

= Automatic Attribute Detection

The widget automatically extracts the data attribute name from the selector:
- "[data-branch-item]" → filters on element.dataset.branchItem
- "[data-repo-item]" → filters on element.dataset.repoItem
- "[data-job-item]" → filters on element.dataset.jobItem

= Filtering Logic

The component automatically:
1. Converts search text to lowercase for case-insensitive matching
2. Shows all items when search is empty
3. Shows items where the filter attribute contains the search text
4. Always shows items with data-*-item="all-*" (for "All Branches" type entries)
5. Hides non-matching items

= Design Guidelines

- Uses design system input styling with focus states
- Includes search icon for visual clarity
- Integrates seamlessly with existing card layouts
- Follows accessibility best practices

= Technical Implementation

Uses hyperscript for client-side filtering to provide instant feedback
without server round-trips. Requires target elements to have appropriate
data attributes for filtering.
-}
viraFilterInput_ :: Text -> [Attributes] -> Html ()
viraFilterInput_ targetSelector attrs = do
  -- Extract attribute name from selector like "[data-branch-item]" -> "branchItem"
  let filterAttribute = case targetSelector of
        s
          | "[data-" `T.isPrefixOf` s && "]" `isSuffixOf` s ->
              let attrName = T.drop 6 (T.take (T.length s - 1) s) -- Remove "[data-" and "]"
                  camelCase = toCamelCase attrName
               in camelCase
        _ -> "item" -- fallback

      -- Convert kebab-case to camelCase (e.g., "branch-item" -> "branchItem")
      toCamelCase :: Text -> Text
      toCamelCase text =
        let parts = splitOn "-" text
         in case parts of
              [] -> text
              (first_ : rest) -> first_ <> mconcat (map capitalize rest)

      capitalize :: Text -> Text
      capitalize t = case T.uncons t of
        Nothing -> t
        Just (c, cs) -> cons (toUpper c) cs
  div_ [class_ "relative"] $ do
    input_
      ( [ type_ "text"
        , class_ "w-full px-3 py-2 text-sm border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 bg-white transition-colors duration-200 pr-10"
        , hyperscript_ $
            "on input "
              <> "set filterText to my.value.toLowerCase() "
              <> "for item in document.querySelectorAll('"
              <> targetSelector
              <> "') "
              <> "set itemValue to item.dataset."
              <> filterAttribute
              <> " "
              <> "if filterText is '' then show item "
              <> "else if itemValue and itemValue.toLowerCase().includes(filterText) then show item "
              <> "else if itemValue and itemValue.includes('all-') then show item "
              <> "else hide item "
              <> "end"
        ]
          <> attrs
      )
    div_ [class_ "absolute inset-y-0 right-0 flex items-center pr-3 pointer-events-none"] $ do
      span_ [class_ "text-gray-400 text-sm"] "🔍"
