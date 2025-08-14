{- |
Button components with type-safe styling variants.
-}
module Vira.Widgets.Button (
  viraButton_,
  viraIconButton_,
  ButtonVariant (..),
) where

import Lucid
import Lucid.Htmx.Extra (hyperscript_)

-- | Button variant types for consistent styling
data ButtonVariant
  = -- | Indigo - primary actions, forms
    ButtonPrimary
  | -- | Red - delete, disconnect, kill actions
    ButtonDestructive
  | -- | Green - build, success actions
    ButtonSuccess
  | -- | Gray - secondary actions
    ButtonSecondary
  deriving stock (Eq, Show)

{- |
Enhanced button component with type-safe styling variants.

This is the primary button component for all user actions. It includes:
- Type-safe variant system for consistent styling
- Smooth transitions and micro-interactions
- Proper focus states for accessibility
- Disabled state handling
- Automatic color coordination

= Usage Examples

@
-- Primary action (most important action on page)
W.viraButton_ W.ButtonPrimary [type_ "submit"] "Save Changes"

-- Success action
W.viraButton_ W.ButtonSuccess [] "✅ Build"

-- Destructive action
W.viraButton_ W.ButtonDestructive [] "🗑️ Delete"

-- Secondary action
W.viraButton_ W.ButtonSecondary [] "Cancel"

-- With additional attributes
W.viraButton_ W.ButtonPrimary [type_ "submit", form_ "my-form"] "Submit"
@

= Variant Guidelines

- **ButtonPrimary**: Main actions, form submissions (indigo)
- **ButtonSuccess**: Positive actions like build, save (green)
- **ButtonDestructive**: Delete, disconnect, kill actions (red)
- **ButtonSecondary**: Less important actions, cancel (gray)

= Type Safety

Colors are automatically managed by the variant type, preventing inconsistent styling.
-}
viraButton_ :: forall {result}. (Term [Attributes] result) => ButtonVariant -> [Attributes] -> result
viraButton_ variant attrs =
  let (colorClasses, focusRing) = case variant of
        ButtonPrimary -> ("bg-indigo-600 hover:bg-indigo-700 text-white", "focus:ring-indigo-500")
        ButtonSuccess -> ("bg-green-600 hover:bg-green-700 text-white", "focus:ring-green-500")
        ButtonDestructive -> ("bg-red-600 hover:bg-red-700 text-white", "focus:ring-red-500")
        ButtonSecondary -> ("bg-gray-600 hover:bg-gray-700 text-white", "focus:ring-gray-500")
   in button_
        ( [ class_ $ "inline-flex items-center justify-center px-6 py-3 text-sm font-semibold rounded-lg transition-smooth focus:outline-none focus:ring-2 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed shadow-md hover:shadow-lg " <> colorClasses <> " " <> focusRing
          , hyperscript_ "on click add .scale-95 then wait 100ms then remove .scale-95"
          ]
            <> attrs
        )

{- |
Icon button variant for secondary actions and toolbar buttons.

Smaller, more subtle button for icon-only actions. Perfect for:
- Toolbar actions
- Settings buttons
- Secondary controls that don't need emphasis

= Usage Examples

@
-- Settings action
W.viraIconButton_ [] "⚙️"

-- Edit action
W.viraIconButton_ [title_ "Edit"] "✏️"

-- Close/cancel action
W.viraIconButton_ [onclick_ "closeModal()"] "✕"
@

= Design Notes

Uses neutral colors by default to avoid competing with primary actions.
Always include a title attribute for accessibility when using icons.
-}
viraIconButton_ :: forall {result}. (Term [Attributes] result) => [Attributes] -> result
viraIconButton_ attrs =
  button_
    ( [ class_ "inline-flex items-center justify-center p-2 text-sm font-medium rounded-lg transition-smooth focus:outline-none focus:ring-2 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed bg-white hover:bg-gray-50 text-gray-700 border border-gray-300 shadow-sm hover:shadow-md focus:ring-indigo-500"
      ]
        <> attrs
    )
