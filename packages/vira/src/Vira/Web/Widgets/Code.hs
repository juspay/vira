{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Code display components with copyable functionality
module Vira.Web.Widgets.Code (
  viraCodeBlockCopyable,
  viraCodeBlockCopyableJs,
  viraCodeInlineCopyable,
) where

import Data.Text qualified as T
import Lucid

-- | Configuration for code display
data ViraCodeConfig = ViraCodeConfig
  { display :: CodeDisplay
  , copyable :: Maybe CopyConfig
  }

-- | How to display code
data CodeDisplay
  = Inline
  | Block BlockStyle

-- | Block display style
data BlockStyle
  = -- | Single line, wraps if needed
    SingleLine
  | -- | Preserves newlines and spacing
    MultiLine

-- | Copy-to-clipboard configuration
data CopyConfig = CopyConfig
  { textToCopy :: Maybe Text
  -- ^ Text to copy (defaults to display text)
  , feedback :: Text
  -- ^ Feedback message (defaults to "Copied!")
  , feedbackDuration :: Int
  -- ^ Duration in milliseconds
  }

-- | Default copy config
defaultCopy :: CopyConfig
defaultCopy =
  CopyConfig
    { textToCopy = Nothing
    , feedback = "Copied!"
    , feedbackDuration = 1000
    }

-- | Main code rendering function
viraCode :: (Monad m) => ViraCodeConfig -> Text -> HtmlT m ()
viraCode cfg txt = case cfg.display of
  Inline -> renderInline cfg txt
  Block style -> renderBlock cfg style txt

renderInline :: (Monad m) => ViraCodeConfig -> Text -> HtmlT m ()
renderInline cfg txt =
  code_ [baseInlineClass <> copyAttrs cfg txt] $ toHtml txt
  where
    baseInlineClass =
      class_ $
        T.intercalate
          " "
          [ "px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700"
          , "text-gray-700 dark:text-gray-300 rounded font-mono"
          , if isJust cfg.copyable
              then "hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors cursor-pointer"
              else ""
          ]

renderBlock :: (Monad m) => ViraCodeConfig -> BlockStyle -> Text -> HtmlT m ()
renderBlock cfg style txt =
  code_ [baseBlockClass <> copyAttrs cfg txt] $ toHtml txt
  where
    baseBlockClass =
      class_ $
        T.intercalate
          " "
          [ "block px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700"
          , "text-gray-700 dark:text-gray-300 rounded font-mono"
          , case style of
              SingleLine -> ""
              MultiLine -> "whitespace-pre-wrap"
          , if isJust cfg.copyable
              then "hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors cursor-pointer"
              else ""
          ]

copyAttrs :: ViraCodeConfig -> Text -> Attributes
copyAttrs cfg displayText = case cfg.copyable of
  Nothing -> mempty
  Just copyCfg ->
    let copyText = fromMaybe displayText copyCfg.textToCopy
     in title_ "Click to copy"
          <> onclick_
            ( "event.stopPropagation(); event.preventDefault(); "
                <> "navigator.clipboard.writeText(`"
                <> copyText
                <> "`); "
                <> "this.innerText = '"
                <> copyCfg.feedback
                <> "'; "
                <> "setTimeout(() => { this.innerText = `"
                <> displayText
                <> "`; }, "
                <> show copyCfg.feedbackDuration
                <> "); "
                <> "return false;"
            )

-- | Block code with copy functionality
viraCodeBlockCopyable :: (Monad m) => Text -> HtmlT m ()
viraCodeBlockCopyable =
  viraCode
    ViraCodeConfig
      { display = Block MultiLine
      , copyable = Just defaultCopy
      }

-- | Copyable code block for JavaScript-populated content
viraCodeBlockCopyableJs :: (Monad m) => Text -> HtmlT m ()
viraCodeBlockCopyableJs elemId =
  code_
    [ id_ elemId
    , class_ "block px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600 text-gray-700 dark:text-gray-300 rounded font-mono whitespace-pre transition-colors cursor-pointer"
    , title_ "Click to copy"
    , onclick_ "navigator.clipboard.writeText(this.textContent); const orig = this.textContent; this.textContent = 'Copied!'; setTimeout(() => { this.textContent = orig; }, 1000);"
    ]
    ""

-- | Inline code with custom copy text (display one thing, copy another)
viraCodeInlineCopyable :: (Monad m) => Text -> Text -> HtmlT m ()
viraCodeInlineCopyable displayText copyText =
  viraCode
    ViraCodeConfig
      { display = Inline
      , copyable =
          Just
            defaultCopy
              { textToCopy = Just copyText
              }
      }
    displayText
