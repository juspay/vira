{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Tool status views for the UI
module Vira.Tool.Status (
  viewToolsStatus,
) where

import Effectful.Reader.Dynamic (ask)
import GH.Auth.Status (AuthStatus (..))
import Lucid
import Vira.App.LinkTo.Type qualified as LinkTo
import Vira.App.Lucid (AppHtml, getLinkUrl)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Tool.Core (ToolData (..))
import Vira.Tool.Type.Tools qualified as Tool
import Web.TablerIcons.Outline qualified as Icon
import Prelude hiding (ask)

-- | View tools status indicator for the status bar
viewToolsStatus :: AppHtml ()
viewToolsStatus = do
  viraRuntimeState <- lift $ ask @ViraRuntimeState
  toolsData <- liftIO $ readTVarIO viraRuntimeState.tools
  let hasError = isToolsError toolsData
      (iconSvg, classes, label :: Text) =
        if hasError
          then (Icon.alert_triangle, "text-red-500 dark:text-red-400", "Tools")
          else (Icon.check, "text-green-500 dark:text-green-400", "Tools")
  toolsUrl <- lift $ getLinkUrl LinkTo.Tools
  a_ [href_ toolsUrl, class_ "flex items-center space-x-2", title_ "Tools Status"] $ do
    div_ [class_ $ "w-4 h-4 flex items-center justify-center " <> classes] $
      toHtmlRaw iconSvg
    span_ [class_ "text-white text-sm font-medium"] $ toHtml label
  where
    isToolsError :: Tool.Tools -> Bool
    isToolsError t =
      isLeft t.attic.status
        || isAuthError t.github.status
    isAuthError :: AuthStatus -> Bool
    isAuthError = \case
      NotAuthenticated -> True
      Authenticated {} -> False
