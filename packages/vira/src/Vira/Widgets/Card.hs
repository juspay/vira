{- |
Card container component with clean, minimal styling.
-}
module Vira.Widgets.Card (
  viraCard_,
  viraCardElevated_,
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

-- For elevated cards with shadow
W.viraCardElevated_ [class_ "p-6"] $ do
  -- Important content that needs emphasis
@
-}
viraCard_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraCard_ attrs =
  div_
    ( [ class_ "bg-white rounded-xl border border-gray-200 overflow-hidden"
      ]
        <> attrs
    )

{- |
Elevated card with subtle shadow for important content.
Use sparingly to create visual hierarchy.
-}
viraCardElevated_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraCardElevated_ attrs =
  div_
    ( [ class_ "bg-white rounded-xl border border-gray-200 overflow-hidden shadow-sm transition-smooth"
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
viraNavigationCard_ :: Text -> Html () -> Html ()
viraNavigationCard_ href title = do
  a_ [href_ href, class_ "group block"] $ do
    viraCard_ [class_ "p-6 hover:shadow-lg transition-all duration-300 group-hover:border-indigo-300"] $ do
      div_ [class_ "flex items-center justify-between"] $ do
        h3_ [class_ "text-xl font-semibold text-gray-900 group-hover:text-indigo-700 transition-colors"] title
        div_ [class_ "text-gray-400 group-hover:text-indigo-500 transition-colors ml-4 w-6 h-6 flex items-center justify-center"] $
          toHtmlRaw Icon.chevron_right
