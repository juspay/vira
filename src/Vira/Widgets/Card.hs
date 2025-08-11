{- |
Card container component with elegant styling.
-}
module Vira.Widgets.Card (
  viraCard_,
) where

import Lucid

{- |
Card container component with elegant styling and hover effects.

The primary container for grouping related content. Features:
- Elegant shadow that lifts on hover
- Rounded corners and subtle borders
- Consistent spacing and overflow handling
- Glass-morphism inspired design

= Usage Examples

@
-- Basic content card
W.viraCard_ [class_ "p-6"] $ do
  h3_ [class_ "text-lg font-semibold mb-4"] "Card Title"
  p_ [class_ "text-gray-600"] "Card content goes here"

-- Repository card with custom padding
W.viraCard_ [class_ "p-4 hover:bg-gray-50"] $ do
  -- Repository details

-- Settings section card
W.viraCard_ [class_ "p-6 mb-6"] $ do
  -- Configuration form
@

= Layout Guidelines

Use consistent padding: p-4 for compact cards, p-6 for standard cards.
Combine with grid layouts for responsive card grids.
-}
viraCard_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraCard_ attrs =
  div_
    ( [ class_ "bg-white rounded-xl border border-gray-200 shadow-elegant hover:shadow-lg transition-smooth overflow-hidden"
      ]
        <> attrs
    )
