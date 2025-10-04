{- |
Card container component with clean, minimal styling.
-}
module Vira.Widgets.Card (
  viraCard_,
  viraNavigationCard_,
) where

import Lucid
import Web.TablerIcons.Outline qualified as Icon

{- |
Basic card container with minimal styling.

Clean, flat design following KISS principles. Features:
- Subtle border and background
- Rounded corners
- Consistent overflow handling

= Usage Examples

@
-- Basic content card
W.viraCard_ [class_ "p-6"] $ do
  h3_ [class_ "text-lg font-semibold mb-4"] "Card Title"
  p_ [class_ "text-gray-600"] "Card content goes here"
@
-}
viraCard_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraCard_ attrs =
  div_
    ( [ class_ "bg-white dark:bg-gray-800 rounded-xl border border-gray-200 dark:border-gray-700 overflow-hidden"
      ]
        <> attrs
    )

{- |
Navigation card with hover effects and chevron icon.

Reusable component for clickable navigation cards with consistent styling:
- Hover effects with shadow and border color transitions
- Tabler chevron-right icon indicator
- Semantic group hover states
- Clean, accessible design

= Usage Examples

@
-- Basic navigation card
W.viraNavigationCard_ "/repositories" "Repositories"

-- Repository listing card
W.viraNavigationCard_ (show repoUrl) (toHtml $ toString repo.name)
@

= Design Guidelines

- Use for clickable navigation elements
- Title should be clear and descriptive
- href should be a valid URL or route
- Keep titles concise for clean appearance
-}
viraNavigationCard_ :: (Monad m) => Text -> HtmlT m () -> HtmlT m ()
viraNavigationCard_ href title = do
  a_ [href_ href, class_ "group block"] $ do
    viraCard_ [class_ "p-6 hover:shadow-lg transition-all duration-300 group-hover:border-indigo-300 dark:group-hover:border-indigo-600"] $ do
      div_ [class_ "flex items-center justify-between"] $ do
        h3_ [class_ "text-xl font-semibold text-gray-900 dark:text-gray-100 group-hover:text-indigo-700 dark:group-hover:text-indigo-400 transition-colors"] title
        div_ [class_ "text-gray-400 dark:text-gray-500 group-hover:text-indigo-500 dark:group-hover:text-indigo-400 transition-colors ml-4 w-6 h-6 flex items-center justify-center"] $
          toHtmlRaw Icon.chevron_right
