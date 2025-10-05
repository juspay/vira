-- | User information utilities
module Vira.Page.Common.User (
  viewUserInfo,
) where

import Lucid
import System.Environment (getEnv)
import Vira.App.Lucid (AppHtml)
import Web.TablerIcons.Outline qualified as Icon

-- | Display current user information with icon
viewUserInfo :: AppHtml ()
viewUserInfo = do
  currentUser <- lift $ liftIO $ toText <$> getEnv "USER"
  span_ [title_ "User", class_ "cursor-help flex items-center gap-1.5"] $ do
    span_ [class_ "w-3.5 h-3.5 flex items-center justify-center"] $ toHtmlRaw Icon.user
    toHtml currentUser
