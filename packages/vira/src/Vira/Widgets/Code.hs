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
  viraCommitInfo_,
) where

import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import Effectful.Git qualified as Git
import Lucid
import Vira.App qualified
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
  code_ [class_ "px-2 py-1 text-xs bg-gray-100 text-gray-700 rounded font-mono"] $ toHtml code

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
  div_ [class_ "flex items-center space-x-2"] $ do
    viraCodeInline_ (T.take 8 $ toText $ Git.unCommitID commitId)
    case maybeCommit of
      Just commit -> do
        unless (T.null commit.commitMessage) $ do
          span_ [class_ "text-sm text-gray-600 truncate"] $ toHtml commit.commitMessage
        unless (T.null commit.commitAuthor) $ do
          span_ [class_ "text-xs text-gray-500"] $ do
            "by " <> toHtml commit.commitAuthor
            unless (T.null commit.commitAuthorEmail) $ do
              " <" <> toHtml commit.commitAuthorEmail <> ">"
        div_ [class_ "text-xs text-gray-400"] $
          toHtml $
            formatTime defaultTimeLocale "%b %d, %Y" commit.commitDate
      Nothing -> do
        span_ [class_ "text-xs text-red-600"] "Commit not found"
