{-# LANGUAGE OverloadedRecordDot #-}

-- | Tools section view for Environment page
module Vira.Web.Pages.EnvironmentPage.Tools (
  viewTools,
) where

import Data.Text qualified as T
import Lucid
import Vira.Tool.Core (ToolData (..), Tools (..))
import Vira.Tool.Core qualified as Tool
import Vira.Tool.Tools.Attic qualified as AtticTool
import Vira.Tool.Tools.GitHub qualified as GitHubTool
import Vira.Web.Lucid (AppHtml)
import Vira.Web.Widgets.Card qualified as W
import Web.TablerIcons.Outline qualified as Icon

viewTools :: AppHtml ()
viewTools = do
  -- Refresh tools data every time the page is loaded
  tools <- lift Tool.refreshTools

  -- Tools Section
  h2_ [class_ "text-xl font-semibold text-gray-900 dark:text-gray-100 mb-4 flex items-center"] $ do
    div_ [class_ "w-5 h-5 mr-2 flex items-center justify-center"] $ toHtmlRaw Icon.tool
    "Tools"
  p_ [class_ "text-gray-600 dark:text-gray-300 mb-4"] "Command-line tools used by Vira jobs"

  div_ [class_ "grid gap-6 md:grid-cols-2 lg:grid-cols-2"] $ do
    viewToolCard tools.attic (AtticTool.viewToolStatus tools.attic.status)
    viewToolCard tools.github (GitHubTool.viewToolStatus tools.github.status)
    viewToolCard tools.omnix mempty
    viewToolCard tools.git mempty
    viewToolCard tools.cachix mempty

-- | View a tool card with its metadata and runtime info
viewToolCard :: (Monad m) => ToolData statusType -> HtmlT m () -> HtmlT m ()
viewToolCard toolData infoHtml = do
  W.viraCard_ [class_ "p-6"] $ do
    div_ [class_ "flex items-start mb-4"] $ do
      viewToolIcon $ mkToolDisplay toolData.name
      div_ [class_ "flex-1"] $ do
        h3_ [class_ "text-xl font-bold text-gray-900 dark:text-gray-100 mb-2"] $ do
          a_
            [ href_ toolData.url
            , target_ "_blank"
            , class_ "inline-flex items-center gap-1 hover:text-indigo-600 dark:hover:text-indigo-400"
            ]
            $ do
              toHtml toolData.name
              span_ [class_ "w-4 h-4 flex items-center"] $ toHtmlRaw Icon.external_link
        p_ [class_ "text-gray-600 dark:text-gray-300 text-sm mb-3"] $ toHtml toolData.description

        -- Render tool-specific info
        infoHtml

        -- Collapsible bin paths (debug info)
        details_ [class_ "mt-3"] $ do
          summary_ [class_ "text-xs text-gray-500 dark:text-gray-400 cursor-pointer hover:text-gray-700 dark:hover:text-gray-300"] "Show binary paths"
          div_ [class_ "mt-2 space-y-1"] $ do
            forM_ toolData.binPaths $ \binPath ->
              code_ [class_ "block text-xs bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 px-2 py-1 rounded font-mono"] $ toHtml binPath

-- | Tool display styling
data ToolDisplay = ToolDisplay
  { initial :: Text
  , bgClass :: Text
  , textClass :: Text
  }

-- | Render tool icon badge
viewToolIcon :: (Monad m) => ToolDisplay -> HtmlT m ()
viewToolIcon disp =
  span_ [class_ $ "h-12 w-12 mr-4 " <> disp.bgClass <> " rounded-lg flex items-center justify-center " <> disp.textClass <> " font-bold text-xl"] $
    toHtml disp.initial

-- | Get display styling for a tool
mkToolDisplay :: Text -> ToolDisplay
mkToolDisplay name = case name of
  "Attic" -> mkDisplay "bg-indigo-100" "text-indigo-600"
  "GitHub" -> mkDisplay "bg-green-100" "text-green-600"
  "Omnix" -> mkDisplay "bg-purple-100" "text-purple-600"
  "Git" -> mkDisplay "bg-orange-100" "text-orange-600"
  "Cachix" -> mkDisplay "bg-blue-100" "text-blue-600"
  _ -> mkDisplay "bg-gray-100" "text-gray-600"
  where
    mkDisplay bg txt = ToolDisplay {initial = T.take 1 name, bgClass = bg, textClass = txt}
