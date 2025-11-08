{- |
Tab navigation components for switching between views.

This module provides tabbed navigation components following the Vira Design System.
-}
module Vira.Web.Widgets.Tabs (
  viraTabs_,
  TabItem (..),
) where

import Lucid

{- | Tab item configuration

Each tab represents a navigable option with optional badge count.
-}
data TabItem = TabItem
  { label :: Text
  -- ^ The display text for the tab
  , href :: Text
  -- ^ The URL this tab navigates to
  , isActive :: Bool
  -- ^ Whether this tab is currently active
  , badge :: Maybe Int
  -- ^ Optional badge count (e.g., for "3 unbuilt branches")
  }

{- | Tabbed navigation bar

Creates a horizontal tab bar with consistent styling following Vira Design System.

= Features

- Active state with indigo accent
- Inactive tabs with hover states
- Optional badge counts
- Full dark mode support
- Responsive design

= Usage

@
viraTabs_ []
  [ TabItem "Builds" "/" True Nothing
  , TabItem "Unbuilt" "/?filter=unbuilt" False (Just 5)
  ]
@

= Design

Active tabs use indigo-600 border and background tint.
Inactive tabs use transparent border with hover effects.
Badges adapt colors based on active/inactive state.
-}
viraTabs_ :: (Monad m) => [Attributes] -> [TabItem] -> HtmlT m ()
viraTabs_ attrs tabs =
  div_ (class_ "border-b-2 border-gray-200 dark:border-gray-700" : attrs) $ do
    div_ [class_ "flex space-x-1 -mb-0.5"] $ do
      forM_ tabs $ \(TabItem {label, href, isActive, badge}) ->
        a_
          [ href_ href
          , class_ $
              "px-4 py-3 font-semibold text-sm transition-colors border-b-2 "
                <> (if isJust badge then "flex items-center gap-2 " else "")
                <> if isActive
                  then "border-indigo-600 dark:border-indigo-400 text-indigo-600 dark:text-indigo-400 bg-indigo-50 dark:bg-indigo-900/20"
                  else "border-transparent text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-gray-200 hover:bg-gray-50 dark:hover:bg-gray-800/50"
          ]
          $ do
            toHtml label
            whenJust badge $ \count ->
              when (count > 0)
                $ span_
                  [ class_ $
                      "inline-flex items-center justify-center px-2 py-0.5 text-xs font-semibold rounded-full "
                        <> if isActive
                          then "bg-indigo-600 dark:bg-indigo-500 text-white"
                          else "bg-gray-200 dark:bg-gray-700 text-gray-700 dark:text-gray-300"
                  ]
                $ toHtml (show count :: String)
