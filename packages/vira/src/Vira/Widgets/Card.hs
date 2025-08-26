{- |
Card container component with clean, minimal styling.
-}
module Vira.Widgets.Card (
  viraCard_,
  viraCardElevated_,
) where

import Lucid

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
