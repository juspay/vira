{- |
Vira Design System - Code Display Components

This module contains code display components for the Vira CI/CD application.
All components follow the Vira Design System guidelines defined in DESIGN.md.

= Component Categories

== Code Display Components
- 'viraCodeBlock_' - Code display with proper formatting for multi-line snippets
- 'viraCodeInline_' - Inline code elements for text integration

= Usage Guidelines

Use these components for displaying code, commands, commit hashes, and technical content.
Always prefer these components over raw HTML to maintain design consistency.

= Design Principles

- Monospace Typography: Proper code font rendering
- Visual Distinction: Clear separation from regular text
- Readability: Appropriate contrast and sizing
- Accessibility: Proper text selection and copying
-}
module Vira.Widgets.Code (
  viraCodeBlock_,
  viraCodeInline_,
) where

import Lucid

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
viraCodeBlock_ :: (Monad m) => Text -> HtmlT m ()
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
viraCodeInline_ :: (Monad m) => Text -> HtmlT m ()
viraCodeInline_ code = do
  code_ [class_ "px-2 py-1 text-xs bg-gray-100 text-gray-700 rounded font-mono"] $ toHtml code
