{- |
Button components with type-safe styling variants.
-}
module Vira.Web.Widgets.Button (
  viraButton_,
  viraButtonIcon_,
  viraRequestButton_,
  ButtonVariant (..),
) where

import Htmx.Lucid.Core (hxSwapS_, hxTarget_)
import Htmx.Lucid.Extra (hxDisabledElt_)
import Htmx.Swap (Swap (InnerHTML))
import Lucid
import Lucid.Htmx.Contrib (hxPostSafe_, hyperscript_)
import Servant.Links (Link)
import Vira.Web.Widgets.Modal (viraGlobalModalId)

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

{- | Enhanced button component with type-safe 'ButtonVariant' styling.

This is the primary button component for all user actions. It includes:
- Type-safe 'ButtonVariant' system for consistent styling
- Smooth transitions and micro-interactions
- Proper focus states for accessibility
- Disabled state handling
- Automatic color coordination

= Usage Examples

@
-- Primary action (most important action on page)
W.viraButton_ W.ButtonPrimary [type_ "submit"] "Save Changes"

-- Success action
W.viraButton_ W.ButtonSuccess [] "Build"

-- Destructive action
W.viraButton_ W.ButtonDestructive [] "Delete"

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
        ButtonSecondary -> ("bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600 text-gray-900 dark:text-gray-100", "focus:ring-gray-500")
   in button_
        ( [ class_ $ "inline-flex items-center justify-center px-6 py-3 text-sm font-semibold rounded-lg transition-colors focus:outline-none focus:ring-2 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed cursor-pointer " <> colorClasses <> " " <> focusRing
          , hyperscript_ "on click add .scale-95 then wait 100ms then remove .scale-95"
          ]
            <> attrs
        )

{- |
Small icon wrapper for button icons (16px with margin).

= Usage Example

@
W.viraButton_ W.ButtonSuccess [] $ do
  viraButtonIcon_ $ toHtmlRaw Icon.player_play
  "Build Branch"
@
-}
viraButtonIcon_ :: forall {result}. (Term [Attributes] result) => result
viraButtonIcon_ =
  div_ [class_ "w-4 h-4 mr-2 flex items-center justify-center"]

{- |
Button for server requests that displays errors in a modal.

Automatically configures htmx POST and modal error handling using the global
modal container ('viraGlobalModalContainer_') defined in the layout.

The button is automatically disabled when clicked to prevent duplicate requests.

= Usage

@
updateLink <- lift $ App.getLink $ LinkTo.RepoUpdate repo.name
W.viraRequestButton_ W.ButtonSecondary updateLink [title_ "Refresh"] $ do
  W.viraButtonIcon_ $ toHtmlRaw Icon.refresh
  "Refresh"
@

Handler returns 'viraErrorModal_' on error, refresh header on success:
@
case result of
  Left err -> App.runAppHtml (W.viraErrorModal_ err) >>= pure . noHeader
  Right () -> pure $ addHeader True mempty
@
-}
viraRequestButton_ ::
  (Monad m) =>
  ButtonVariant ->
  -- | Endpoint to POST to
  Link ->
  [Attributes] ->
  HtmlT m () ->
  HtmlT m ()
viraRequestButton_ buttonType endpoint attrs content = do
  let allAttrs =
        [ hxPostSafe_ endpoint
        , hxTarget_ ("#" <> viraGlobalModalId)
        , hxSwapS_ InnerHTML
        , -- Disable button on click to prevent duplicate requests
          hxDisabledElt_ "this"
        ]
          <> attrs
  viraButton_ buttonType allAttrs content
