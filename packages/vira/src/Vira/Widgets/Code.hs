{-# LANGUAGE OverloadedRecordDot #-}

-- | Code display components with copyable functionality
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

-- | Inline code element for short snippets
viraCodeInline_ :: (Monad m) => Text -> HtmlT m ()
viraCodeInline_ code = do
  code_ [class_ "px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded font-mono"] $ toHtml code

-- | Attributes for click-to-copy functionality. Shows "Copied!" feedback for 1s.
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

-- | Block code element with click-to-copy functionality
viraCodeCopyable_ :: (Monad m) => Text -> HtmlT m ()
viraCodeCopyable_ code = do
  code_
    [ class_ "block px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600 text-gray-700 dark:text-gray-300 rounded font-mono transition-colors cursor-pointer"
        <> copyable code code
    ]
    $ toHtml code

-- | Commit info: hash, message, author, date
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

-- | Compact commit info: hash, message, relative time
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

-- | Clickable commit hash (8 chars) with copy-to-clipboard
viraCommitHash_ :: Git.CommitID -> Vira.App.AppHtml ()
viraCommitHash_ commitId = do
  let shortHash = T.take 8 $ toText $ Git.unCommitID commitId
      fullHash = toText $ Git.unCommitID commitId
  code_
    [ class_ "px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600 text-gray-700 dark:text-gray-300 rounded font-mono transition-colors cursor-pointer"
        <> copyable fullHash shortHash
    ]
    $ toHtml shortHash
