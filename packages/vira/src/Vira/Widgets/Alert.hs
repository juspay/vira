{- |
Alert component for displaying important messages with semantic colors.
-}
module Vira.Widgets.Alert (
  viraAlert_,
  AlertType (..),
) where

import Lucid
import Web.TablerIcons.Outline qualified as Icon

-- | Alert types for consistent messaging
data AlertType
  = AlertSuccess
  | AlertError
  | AlertWarning
  | AlertInfo
  deriving stock (Eq, Show)

{- |
Alert component for displaying important messages with semantic colors.

Provides user feedback with appropriate visual styling and icons.
Includes accessibility features with proper ARIA roles.
Now type-safe with automatic color management based on alert type.

= Usage Examples

@
-- Success message
W.viraAlert_ W.AlertSuccess $ do
  p_ [class_ "text-green-800"] "Repository successfully added!"

-- Error message
W.viraAlert_ W.AlertError $ do
  p_ [class_ "text-red-800"] "Failed to connect to repository"

-- Warning message
W.viraAlert_ W.AlertWarning $ do
  p_ [class_ "text-yellow-800"] "This action cannot be undone"

-- Info message
W.viraAlert_ W.AlertInfo $ do
  p_ [class_ "text-blue-800"] "Configure Cachix to enable build caching"
@

= Color Management

Colors are automatically applied based on alert type:
- Green: Success messages (bg-green-50 border-green-200)
- Red: Error messages (bg-red-50 border-red-200)
- Yellow: Warning messages (bg-yellow-50 border-yellow-200)
- Blue: Info messages (bg-blue-50 border-blue-200)

= Icons

Automatically includes appropriate Tabler SVG icons:
- check for success alerts
- x for error alerts
- alert_triangle for warning alerts
- info_circle for info alerts

= Type Safety

Uses 'AlertType' ADT to prevent invalid alert types at compile time.
No manual color class management required.

= Accessibility

Includes role="alert" for screen readers.
-}
viraAlert_ :: (Monad m) => AlertType -> HtmlT m () -> HtmlT m ()
viraAlert_ alertType content = do
  let (colorClass, iconSvg, iconColor) = case alertType of
        AlertError -> ("bg-red-50 dark:bg-red-900/20 border-red-200 dark:border-red-800", Icon.x, "text-red-500 dark:text-red-400")
        AlertWarning -> ("bg-yellow-50 dark:bg-yellow-900/20 border-yellow-200 dark:border-yellow-800", Icon.alert_triangle, "text-yellow-500 dark:text-yellow-400")
        AlertSuccess -> ("bg-green-50 dark:bg-green-900/20 border-green-200 dark:border-green-800", Icon.check, "text-green-500 dark:text-green-400")
        AlertInfo -> ("bg-blue-50 dark:bg-blue-900/20 border-blue-200 dark:border-blue-800", Icon.info_circle, "text-blue-500 dark:text-blue-400")
  div_ [class_ $ "rounded-lg p-4 border " <> colorClass, role_ "alert"] $ do
    div_ [class_ "flex items-start"] $ do
      div_ [class_ "flex-shrink-0"] $ do
        div_ [class_ $ iconColor <> " w-5 h-5 flex items-center justify-center"] $ toHtmlRaw iconSvg
      div_ [class_ "ml-3 flex-1"] content
