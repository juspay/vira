{-# LANGUAGE OverloadedRecordDot #-}

-- | Events debug page - view recent events from event bus
module Vira.Web.Pages.EventsPage (
  Routes (..),
  handlers,
) where

import Data.Acid.Events (SomeUpdate (..))
import Data.Acid.Events qualified as Events
import Effectful.Reader.Dynamic qualified as Reader
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.Type (ViraRuntimeState (..))
import Vira.State.Core (ViraState)
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, runAppHtml)
import Vira.Web.Stack qualified as Web
import Vira.Web.Widgets.Layout qualified as W
import Vira.Web.Widgets.Time qualified as W
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
viewHandler = W.layout [LinkTo.Events] viewEvents

viewEvents :: AppHtml ()
viewEvents = do
  bus <- lift $ Reader.asks @ViraRuntimeState (.eventBus)
  events <- liftIO $ Events.getRecentEvents bus
  W.viraSection_ [] $ do
    W.viraPageHeaderWithIcon_ (toHtmlRaw Icon.bell) "Recent Events" $ do
      p_ [class_ "text-gray-600 dark:text-gray-300"] $
        "Last " <> toHtml (show (length events) :: Text) <> " events from the event bus"

    if null events
      then emptyState
      else eventsList events
  where
    emptyState =
      div_ [class_ "bg-white dark:bg-gray-800 rounded-lg shadow-sm border border-gray-200 dark:border-gray-700 p-12"] $ do
        div_ [class_ "flex flex-col items-center justify-center text-center space-y-4"] $ do
          div_ [class_ "w-16 h-16 text-gray-400 dark:text-gray-500"] $ toHtmlRaw Icon.bell_off
          p_ [class_ "text-lg font-medium text-gray-900 dark:text-gray-100"] "No events yet"
          p_
            [class_ "text-sm text-gray-600 dark:text-gray-400"]
            "Events will appear here as they occur on the event bus"

    eventsList events =
      div_ [class_ "space-y-3"] $
        forM_ events renderEvent

renderEvent :: SomeUpdate ViraState -> AppHtml ()
renderEvent (SomeUpdate evt _result timestamp) = do
  div_ [class_ "bg-white dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700 hover:border-gray-300 dark:hover:border-gray-600 transition-colors"] $ do
    div_ [class_ "p-4"] $ do
      div_ [class_ "flex items-start justify-between gap-4"] $ do
        -- Left: Event
        div_ [class_ "flex-1 min-w-0"] $ do
          code_ [class_ "text-sm text-gray-900 dark:text-gray-100 bg-gray-50 dark:bg-gray-900 px-3 py-2 rounded block overflow-x-auto"] $
            toHtml (show evt :: Text)

        -- Right: Timestamp
        div_ [class_ "flex-shrink-0"] $
          W.viraRelativeTime_ timestamp
