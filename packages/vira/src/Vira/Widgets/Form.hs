{- |
Vira Design System - Form Components

This module contains form components for the Vira CI/CD application.
All components follow the Vira Design System guidelines defined in DESIGN.md.

= Component Categories

== Form Components
- 'viraInput_' - Form input fields with proper focus states
- 'viraLabel_' - Form labels with consistent typography
- 'viraFormGroup_' - Form field grouping for consistent layouts
- 'viraFilterInput_' - Real-time content filtering input

= Usage Guidelines

Use these components for all form interactions to maintain design consistency.
Always pair labels with inputs using proper accessibility attributes.

= Design Principles

- Accessibility First: Proper label-input associations
- Consistent Styling: Unified visual appearance
- Focus Management: Clear focus states and transitions
- Responsive Design: Works across all screen sizes
-}
module Vira.Widgets.Form (
  viraInput_,
  viraLabel_,
  viraFormGroup_,
  viraFilterInput_,
) where

import Data.Char (toUpper)
import Data.Text (cons, isSuffixOf, splitOn)
import Data.Text qualified as T
import Lucid
import Lucid.Htmx.Contrib (hyperscript_)
import Web.TablerIcons.Outline qualified as Icon

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
4. Hides non-matching items

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
              <> "else hide item "
              <> "end"
        ]
          <> attrs
      )
    div_ [class_ "absolute inset-y-0 right-0 flex items-center pr-3 pointer-events-none"] $ do
      div_ [class_ "text-gray-400 w-4 h-4 flex items-center justify-center"] $ toHtmlRaw Icon.search
