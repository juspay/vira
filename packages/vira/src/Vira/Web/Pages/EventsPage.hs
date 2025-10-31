-- | Events debug page - view recent events from event bus
module Vira.Web.Pages.EventsPage (
  Routes (..),
  handlers,
) where

import Data.Time.Format (defaultTimeLocale, formatTime)
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Vira.App qualified as App
import Vira.App.CLI (WebSettings)
import Vira.App.Event (TimestampedUpdate (..))
import Vira.App.Event qualified as Event
import Vira.App.Event.Type (SomeUpdate (..))
import Vira.Web.LinkTo.Type qualified as LinkTo
import Vira.Web.Lucid (AppHtml, runAppHtml)
import Vira.Web.Stack qualified as Web
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
viewHandler = W.layout [LinkTo.Events] viewEvents

viewEvents :: AppHtml ()
viewEvents = do
  events <- lift Event.getRecentEvents
  W.viraSection_ [] $ do
    W.viraPageHeaderWithIcon_ (toHtmlRaw Icon.bell) "Recent Events" $ do
      p_ [class_ "text-gray-600 dark:text-gray-300"] $
        "Last " <> toHtml (show (length events) :: Text) <> " events from the event bus"

    -- Events table
    div_ [class_ "bg-white dark:bg-gray-800 rounded-lg shadow-sm border border-gray-200 dark:border-gray-700 overflow-hidden"] $ do
      div_ [class_ "overflow-x-auto"] $ do
        table_ [class_ "min-w-full divide-y divide-gray-200 dark:divide-gray-700"] $ do
          thead_ [class_ "bg-gray-50 dark:bg-gray-900"] $ do
            tr_ $ do
              th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider"] "Timestamp"
              th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider"] "Event"

          tbody_ [class_ "bg-white dark:bg-gray-800 divide-y divide-gray-200 dark:divide-gray-700"] $ do
            if null events
              then tr_ $ td_ [colspan_ "2", class_ "px-6 py-4 text-center text-gray-500 dark:text-gray-400"] "No events yet"
              else forM_ events renderEvent

renderEvent :: (Monad m) => TimestampedUpdate -> HtmlT m ()
renderEvent (TimestampedUpdate time (SomeUpdate update _result)) = do
  tr_ [class_ "hover:bg-gray-50 dark:hover:bg-gray-700 transition-colors"] $ do
    td_ [class_ "px-6 py-4 whitespace-nowrap text-sm text-gray-500 dark:text-gray-400"] $ do
      code_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
    td_ [class_ "px-6 py-4 text-sm text-gray-900 dark:text-gray-100"] $ do
      code_ [class_ "text-xs bg-gray-100 dark:bg-gray-900 px-2 py-1 rounded"] $ toHtml (show update :: Text)
