{-# LANGUAGE OverloadedRecordDot #-}

-- | Builders page HTTP handlers and views
module Vira.Web.Pages.EnvironmentPage.Builders (
  Routes (..),
  handlers,
) where

import Data.Text qualified as T
import Effectful.Error.Static (runErrorNoCallStack)
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import System.Nix.Config (NixConfig (..), RemoteBuilder (..), nixConfigShow)
import System.Nix.System (System (..))
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, runAppHtml)
import Vira.Web.Pages.Common.User qualified as User
import Vira.Web.Pages.EnvironmentPage.Navigation qualified as Navigation
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Card qualified as W
import Vira.Web.Widgets.Layout qualified as W
import Web.TablerIcons.Outline qualified as Icon

newtype Routes mode = Routes
  { _view :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

handlers :: App.GlobalSettings -> App.ViraRuntimeState -> WebSettings -> Routes AsServer
handlers globalSettings viraRuntimeState webSettings =
  Routes
    { _view = Web.runAppInServant globalSettings viraRuntimeState webSettings . runAppHtml $ viewHandler
    }

viewHandler :: AppHtml ()
viewHandler = W.layout [LinkTo.Environment] viewEnvironment

viewEnvironment :: AppHtml ()
viewEnvironment = do
  W.viraSection_ [] $ do
    W.viraPageHeaderWithIcon_ (toHtmlRaw Icon.world) "Environment" $ do
      div_ [class_ "flex items-center justify-between"] $ do
        p_ [class_ "text-gray-600 dark:text-gray-300"] "Runtime environment and infrastructure"
        span_ [class_ "text-indigo-800 dark:text-indigo-300 font-semibold"] User.viewUserInfo

    -- Navigation tabs
    Navigation.viewEnvironmentTabs False

    -- Builders Section
    h2_ [class_ "text-xl font-semibold text-gray-900 dark:text-gray-100 mb-4 flex items-center"] $ do
      div_ [class_ "w-5 h-5 mr-2 flex items-center justify-center"] $ toHtmlRaw Icon.server
      "Remote Builders"
    p_ [class_ "text-gray-600 dark:text-gray-300 mb-4"] "Distributed build infrastructure from Nix configuration"

    -- Fetch nix config and handle errors
    result <- lift $ runErrorNoCallStack nixConfigShow
    case result of
      Left err -> viewErrorState err
      Right nixConfig -> do
        -- Debug: show raw config
        details_ [class_ "mb-4"] $ do
          summary_ [class_ "text-xs text-gray-500 dark:text-gray-400 cursor-pointer"] "Debug: Raw Nix Config"
          pre_ [class_ "mt-2 text-xs bg-gray-100 dark:bg-gray-800 p-2 rounded overflow-x-auto"] $
            toHtml (show nixConfig :: Text)

        if null nixConfig.builders
          then viewEmptyState
          else viewBuildersTable nixConfig.builders

-- | View error state when config loading fails
viewErrorState :: (Monad m) => Text -> HtmlT m ()
viewErrorState err = do
  W.viraCard_ [class_ "p-8 border-2 border-red-200 dark:border-red-800"] $ do
    div_ [class_ "w-16 h-16 mx-auto mb-4 text-red-500 dark:text-red-400"] $ toHtmlRaw Icon.alert_triangle
    p_ [class_ "text-gray-900 dark:text-gray-100 mb-2 font-semibold"] "Failed to load Nix configuration"
    details_ [class_ "mt-3"] $ do
      summary_ [class_ "text-sm text-gray-600 dark:text-gray-400 cursor-pointer hover:text-gray-800 dark:hover:text-gray-300"] "Show error details"
      pre_ [class_ "mt-2 text-xs bg-gray-100 dark:bg-gray-800 text-gray-700 dark:text-gray-300 p-3 rounded overflow-x-auto"] $ toHtml err

-- | View empty state when no builders are configured
viewEmptyState :: (Monad m) => HtmlT m ()
viewEmptyState = do
  W.viraCard_ [class_ "p-8 text-center"] $ do
    div_ [class_ "w-16 h-16 mx-auto mb-4 text-gray-400 dark:text-gray-500"] $ toHtmlRaw Icon.server_off
    p_ [class_ "text-gray-600 dark:text-gray-300 mb-2"] "No remote builders configured"
    p_ [class_ "text-sm text-gray-500 dark:text-gray-400"] "Configure builders in your Nix configuration to enable distributed builds"

-- | View builders in a responsive table
viewBuildersTable :: (Monad m) => [RemoteBuilder] -> HtmlT m ()
viewBuildersTable builders = do
  W.viraCard_ [class_ "overflow-hidden"] $ do
    div_ [class_ "overflow-x-auto"] $ do
      table_ [class_ "min-w-full divide-y divide-gray-200 dark:divide-gray-700"] $ do
        thead_ [class_ "bg-gray-50 dark:bg-gray-800"] $ do
          tr_ $ do
            th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider"] "URI"
            th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider"] "Platforms"
            th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider"] "Jobs"
            th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider"] "Speed"
            th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider"] "Features"
        tbody_ [class_ "bg-white dark:bg-gray-900 divide-y divide-gray-200 dark:divide-gray-700"] $ do
          forM_ builders viewBuilderRow

-- | View a single builder row
viewBuilderRow :: (Monad m) => RemoteBuilder -> HtmlT m ()
viewBuilderRow builder = do
  tr_ [class_ "hover:bg-gray-50 dark:hover:bg-gray-800"] $ do
    -- URI
    td_ [class_ "px-6 py-4"] $ do
      div_ [class_ "text-sm font-medium text-gray-900 dark:text-gray-100 font-mono"] $ toHtml builder.uri
      whenJust builder.sshKey $ \key ->
        div_ [class_ "text-xs text-gray-500 dark:text-gray-400 mt-1"] $ do
          span_ [class_ "inline-flex items-center"] $ do
            div_ [class_ "w-3 h-3 mr-1"] $ toHtmlRaw Icon.key
            toHtml key

    -- Platforms
    td_ [class_ "px-6 py-4"] $ do
      div_ [class_ "flex flex-wrap gap-1"] $ do
        forM_ builder.platforms $ \(System platform) ->
          span_ [class_ "inline-flex items-center px-2 py-1 rounded text-xs font-medium bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200"] $
            toHtml platform

    -- Max Jobs
    td_ [class_ "px-6 py-4 whitespace-nowrap"] $ do
      span_ [class_ "text-sm text-gray-900 dark:text-gray-100 font-semibold"] $ toHtml (show builder.maxJobs :: Text)

    -- Speed Factor
    td_ [class_ "px-6 py-4 whitespace-nowrap"] $ do
      span_ [class_ "text-sm text-gray-900 dark:text-gray-100"] $ toHtml (show builder.speedFactor :: Text)

    -- Features
    td_ [class_ "px-6 py-4"] $ do
      unless (null builder.supportedFeatures && null builder.mandatoryFeatures) $ do
        div_ [class_ "space-y-1"] $ do
          unless (null builder.supportedFeatures) $ do
            div_ [class_ "text-xs"] $ do
              span_ [class_ "text-gray-500 dark:text-gray-400"] "Supported: "
              span_ [class_ "text-gray-700 dark:text-gray-300"] $ toHtml $ T.intercalate ", " builder.supportedFeatures
          unless (null builder.mandatoryFeatures) $ do
            div_ [class_ "text-xs"] $ do
              span_ [class_ "text-gray-500 dark:text-gray-400"] "Mandatory: "
              span_ [class_ "text-gray-700 dark:text-gray-300"] $ toHtml $ T.intercalate ", " builder.mandatoryFeatures
