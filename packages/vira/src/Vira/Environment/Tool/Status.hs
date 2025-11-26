{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Tool status views for the UI
module Vira.Environment.Tool.Status (
  viewToolsStatus,
) where

import BB.Auth.Status qualified as BB
import Data.Map.Strict qualified as Map
import Effectful.Reader.Dynamic (ask)
import GH.Auth.Status (AuthStatus (..))
import Lucid
import Vira.App.Type (ViraRuntimeState (..))
import Vira.Environment.Tool.Core (ToolData (..))
import Vira.Environment.Tool.Type.Tools qualified as Tool
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, getLinkUrl)
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
          then (Icon.alert_triangle, "text-red-500 dark:text-red-400", "Environment")
          else (Icon.check, "text-green-500 dark:text-green-400", "Environment")
  toolsUrl <- lift $ getLinkUrl LinkTo.Environment
  a_ [href_ toolsUrl, class_ "flex items-center space-x-2", title_ "Tools Status"] $ do
    div_ [class_ $ "w-4 h-4 flex items-center justify-center " <> classes] $
      toHtmlRaw iconSvg
    span_ [class_ "text-white text-sm font-medium"] $ toHtml label
  where
    isToolsError :: Tool.Tools -> Bool
    isToolsError t =
      isLeft t.attic.status
        || isAuthError t.github.status
        || isBitbucketError t.bitbucket.status
    isAuthError :: AuthStatus -> Bool
    isAuthError = \case
      NotAuthenticated -> True
      Authenticated {} -> False
    isBitbucketError :: BB.AuthStatus -> Bool
    isBitbucketError = \case
      BB.NotAuthenticated -> True
      BB.Authenticated servers -> Map.null servers
