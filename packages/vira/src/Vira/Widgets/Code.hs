{-# LANGUAGE OverloadedRecordDot #-}

{- |
Vira Design System - Code Display Components

This module contains code display components for the Vira CI/CD application.
All components follow the Vira Design System guidelines defined in DESIGN.md.

= Component Categories

== Code Display Components
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
  viraCodeInline_,
  viraCodeCopyable_,
  copyable,
  viraCommitInfo_,
  viraCommitInfoCompact_,
  viraCommitHash_,
) where

import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Effectful.Git qualified as Git
import Lucid
import Vira.App qualified
import Vira.Lib.TimeExtra (formatRelativeTime)
import Vira.State.Acid qualified

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

Uses smaller, more subtle styling than block code.
Integrates seamlessly with surrounding text flow.
-}
viraCodeInline_ :: (Monad m) => Text -> HtmlT m ()
viraCodeInline_ code = do
  code_ [class_ "px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded font-mono"] $ toHtml code

{- |
Generate copyable attributes for click-to-copy functionality.

Returns Lucid attributes that add clipboard copy behavior to any element.
Shows "Copied!" feedback for 1 second after copying.

= Parameters
- @textToCopy@: The text to copy to clipboard
- @displayText@: The text to show in the element (may differ from copied text)

= Usage Examples

@
-- Simple copyable button
button_ (copyable "full text here" "short") $ "short"

-- Copyable code
code_ (class_ "..." : copyable commitHash shortHash) $ toHtml shortHash
@
-}
copyable :: Text -> Text -> Attributes
copyable textToCopy displayText =
  title_ "Click to copy"
    <> onclick_
      ( "event.stopPropagation(); event.preventDefault(); "
          <> "navigator.clipboard.writeText('"
          <> textToCopy
          <> "'); "
          <> "this.innerText = 'Copied!'; "
          <> "setTimeout(() => { this.innerText = '"
          <> displayText
          <> "'; }, 1000); "
          <> "return false;"
      )

{- |
Copyable code component with click-to-copy functionality.

Displays code that can be clicked to copy to clipboard.
Shows "Copied!" feedback for 1 second after copying.

= Usage Examples

@
-- Command suggestion
W.viraCodeCopyable_ "attic login myserver https://cache.example.com <token>"

-- File path
W.viraCodeCopyable_ "~/.config/attic/config.toml"
@

= Design Guidelines

Uses button styling with hover effects and clipboard functionality.
Provides clear visual feedback when text is copied.
-}
viraCodeCopyable_ :: (Monad m) => Text -> HtmlT m ()
viraCodeCopyable_ code = do
  button_
    [ class_ "px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600 text-gray-700 dark:text-gray-300 rounded font-mono transition-colors cursor-pointer border-none text-left"
        <> copyable code code
    ]
    $ toHtml code

{- |
Commit information display component showing hash, message, author, and date.

Displays git commit information in a consistent format across the application.
Shows the first 8 characters of the commit hash, commit message (if present),
author with email (if present), and formatted commit date.

= Usage Examples

@
-- In branch listing
div_ $ do
  W.viraCommitInfo_ branch.headCommit

-- In job details
div_ $ do
  "Build commit: "
  W.viraCommitInfo_ job.jobCommit
@

= Design Guidelines

Uses inline code styling for commit hash to maintain visual consistency.
Commit message uses subtle gray text with truncation for long messages.
Date uses smaller text size for secondary information hierarchy.
-}
viraCommitInfo_ :: Git.CommitID -> Vira.App.AppHtml ()
viraCommitInfo_ commitId = do
  maybeCommit <- lift $ Vira.App.query $ Vira.State.Acid.GetCommitByIdA commitId
  div_ [class_ "flex items-center space-x-2 min-w-0"] $ do
    viraCommitHash_ commitId
    case maybeCommit of
      Just commit -> do
        unless (T.null commit.message) $ do
          span_ [class_ "text-sm text-gray-600 dark:text-gray-300 truncate min-w-0 max-w-sm"] $ toHtml commit.message
        unless (T.null commit.author) $ do
          span_ [class_ "text-xs text-gray-500 dark:text-gray-400"] $ do
            "by " <> toHtml commit.author
            unless (T.null commit.authorEmail) $ do
              " <" <> toHtml commit.authorEmail <> ">"
        div_ [class_ "text-xs text-gray-400 dark:text-gray-500"] $
          toHtml $
            formatTime defaultTimeLocale "%b %d, %Y" commit.date
      Nothing -> do
        span_ [class_ "text-xs text-red-600 dark:text-red-400"] "Commit not found"

{- |
Compact commit information component for space-constrained layouts.

Shows only commit hash and message with relative timestamp.
Perfect for list items where space is limited.
-}
viraCommitInfoCompact_ :: Git.CommitID -> Vira.App.AppHtml ()
viraCommitInfoCompact_ commitId = do
  maybeCommit <- lift $ Vira.App.query $ Vira.State.Acid.GetCommitByIdA commitId
  now <- liftIO getCurrentTime
  div_ [class_ "flex items-center space-x-2"] $ do
    viraCommitHash_ commitId
    case maybeCommit of
      Just commit -> do
        unless (T.null commit.message) $ do
          span_ [class_ "text-sm text-gray-700 dark:text-gray-300 truncate max-w-xs"] $ toHtml commit.message
        div_ [class_ "text-xs text-gray-500 dark:text-gray-400"] $
          toHtml $
            formatRelativeTime now commit.date
      Nothing -> do
        span_ [class_ "text-xs text-red-600 dark:text-red-400"] "Commit not found"

{- |
Clickable commit hash component with copy functionality.

Displays a commit hash that can be clicked to copy to clipboard.
-}
viraCommitHash_ :: Git.CommitID -> Vira.App.AppHtml ()
viraCommitHash_ commitId = do
  let shortHash = T.take 8 $ toText $ Git.unCommitID commitId
      fullHash = toText $ Git.unCommitID commitId
  button_
    [ class_ "px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600 text-gray-700 dark:text-gray-300 rounded font-mono transition-colors cursor-pointer border-none"
        <> copyable fullHash shortHash
    ]
    $ toHtml shortHash
