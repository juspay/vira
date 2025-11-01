{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Code display components with copyable functionality
module Vira.Web.Widgets.Code (
  viraCodeBlockCopyable,
  viraCodeInlineCopyable,
) where

import Data.Text qualified as T
import Lucid

-- | Configuration for code display
data ViraCodeConfig = ViraCodeConfig
  { display :: CodeDisplay
  -- ^ How to display the code ('CodeDisplay')
  , copyable :: Maybe CopyConfig
  -- ^ Optional copy-to-clipboard configuration
  , elementId :: Maybe Text
  -- ^ Optional element ID
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
  -- ^ Feedback message (defaults to @Copied!@)
  , feedbackDuration :: Int
  -- ^ Duration in milliseconds
  }

-- | Default 'CopyConfig'
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
  code_ (idAttr <> [baseBlockClass <> copyAttrs cfg txt]) $ toHtml txt
  where
    idAttr = maybe [] (\eid -> [id_ eid]) cfg.elementId
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
        needsCustomCopy = isJust copyCfg.textToCopy
        -- Escape for JavaScript template literals
        escapeJs t = T.replace "\\" "\\\\" $ T.replace "`" "\\`" t
     in title_ "Click to copy"
          <> onclick_
            ( "event.stopPropagation(); event.preventDefault(); "
                <> "const orig = this.textContent; "
                <> (if needsCustomCopy then "navigator.clipboard.writeText(`" <> escapeJs copyText <> "`); " else "navigator.clipboard.writeText(this.textContent); ")
                <> "this.textContent = '"
                <> copyCfg.feedback
                <> "'; "
                <> "setTimeout(() => { this.textContent = orig; }, "
                <> show copyCfg.feedbackDuration
                <> "); "
                <> "return false;"
            )

-- | Block code with copy functionality
viraCodeBlockCopyable :: (Monad m) => Maybe Text -> Text -> HtmlT m ()
viraCodeBlockCopyable mId =
  viraCode
    ViraCodeConfig
      { display = Block MultiLine
      , copyable = Just defaultCopy
      , elementId = mId
      }

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
      , elementId = Nothing
      }
    displayText
