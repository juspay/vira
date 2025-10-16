-- | Shared navigation for Environment pages
module Vira.Web.Pages.EnvironmentPage.Navigation (
  viewEnvironmentTabs,
) where

import Lucid
import Vira.Web.LinkTo.Type (LinkTo (..))
import Vira.Web.Lucid (AppHtml, getLinkUrl)

-- | Navigation tabs for Environment subsections
viewEnvironmentTabs :: Bool -> AppHtml ()
viewEnvironmentTabs isToolsActive = do
  toolsUrl <- lift $ getLinkUrl EnvironmentTools
  buildersUrl <- lift $ getLinkUrl EnvironmentBuilders
  div_ [class_ "border-b border-gray-200 dark:border-gray-700 mb-6"] $ do
    nav_ [class_ "flex space-x-8"] $ do
      -- Tools tab
      a_
        [ class_ $
            "pb-4 px-1 border-b-2 font-medium text-sm "
              <> if isToolsActive
                then "border-indigo-500 text-indigo-600 dark:text-indigo-400"
                else "border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300 dark:text-gray-400 dark:hover:text-gray-300"
        , href_ toolsUrl
        ]
        "Tools"
      -- Builders tab
      a_
        [ class_ $
            "pb-4 px-1 border-b-2 font-medium text-sm "
              <> if not isToolsActive
                then "border-indigo-500 text-indigo-600 dark:text-indigo-400"
                else "border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300 dark:text-gray-400 dark:hover:text-gray-300"
        , href_ buildersUrl
        ]
        "Builders"
