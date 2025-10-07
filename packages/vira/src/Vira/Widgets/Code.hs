-- | Code display components with copyable functionality
module Vira.Widgets.Code (
  viraCodeInline_,
  viraCodeCopyable_,
  copyable,
) where

import Lucid

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
          <> "navigator.clipboard.writeText(`"
          <> textToCopy
          <> "`); "
          <> "this.innerText = 'Copied!'; "
          <> "setTimeout(() => { this.innerText = `"
          <> displayText
          <> "`; }, 1000); "
          <> "return false;"
      )

-- | Block code element with click-to-copy functionality
viraCodeCopyable_ :: (Monad m) => Text -> HtmlT m ()
viraCodeCopyable_ code = do
  code_
    [ class_ "block px-2 py-1 text-xs bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600 text-gray-700 dark:text-gray-300 rounded font-mono transition-colors cursor-pointer whitespace-pre-wrap"
        <> copyable code code
    ]
    $ toHtml code
