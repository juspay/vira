{- |
Modal components for displaying overlay content.
-}
module Vira.Widgets.Modal (
  viraGlobalModalContainer_,
  viraGlobalModalId,
  ErrorModal (..),
) where

import Lucid
import Vira.Widgets.Alert qualified as W
import Web.TablerIcons.Outline qualified as Icon

-- | Type-safe wrapper for error modal content
newtype ErrorModal = ErrorModal {errorMessage :: Text}

instance ToHtml ErrorModal where
  toHtmlRaw = toHtml
  toHtml (ErrorModal errorMsg) = renderErrorModal errorMsg

instance ToHtml (Maybe ErrorModal) where
  toHtmlRaw = toHtml
  toHtml Nothing = mempty
  toHtml (Just modal) = toHtml modal

renderErrorModal :: (Monad m) => Text -> HtmlT m ()
renderErrorModal errorMsg =
  -- Fixed overlay backdrop
  div_
    [ class_ "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50"
    , onclick_ "this.remove()" -- Click backdrop to close
    ]
    $ do
      -- Modal content
      div_
        [ class_ "bg-white rounded-lg shadow-xl max-w-2xl w-full mx-4 max-h-[80vh] overflow-auto"
        , onclick_ "event.stopPropagation()" -- Prevent closing when clicking modal content
        ]
        $ do
          -- Header
          div_ [class_ "flex items-center justify-between p-6 border-b border-gray-200"] $ do
            div_ [class_ "flex items-center space-x-3"] $ do
              div_ [class_ "w-8 h-8 bg-red-100 rounded-full flex items-center justify-center"] $
                div_ [class_ "w-5 h-5 text-red-600"] $
                  toHtmlRaw Icon.alert_triangle
              h3_ [class_ "text-xl font-semibold text-gray-900"] "Error"
            button_
              [ class_ "text-gray-400 hover:text-gray-600"
              , onclick_ "this.closest('.fixed').remove()"
              , type_ "button"
              ]
              $ div_ [class_ "w-6 h-6"]
              $ toHtmlRaw Icon.x

          -- Body
          div_ [class_ "p-6"] $ do
            W.viraAlert_ W.AlertError $
              pre_ [class_ "text-sm text-red-800 font-mono whitespace-pre-wrap break-words"] $
                toHtml errorMsg

-- | Global modal container ID used throughout the application
viraGlobalModalId :: Text
viraGlobalModalId = "vira-modal"

{- |
Global modal container for the application.

This should be rendered once in the layout (typically in the body, before main content).
All request buttons using 'viraRequestButton_' will target this container.
-}
viraGlobalModalContainer_ :: (Monad m) => HtmlT m ()
viraGlobalModalContainer_ = div_ [id_ viraGlobalModalId] mempty
