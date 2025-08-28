-- | Simple event logger that prints events to stdout
module Vira.Event.Logger (
  startEventLogger,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.CircularBuffer qualified as CB
import Data.ByteString qualified as ByteString
import Data.Time (defaultTimeLocale, formatTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic (Reader)
import Vira.App.Stack (AppState)
import Vira.Event.Broadcast qualified as Broadcast
import Vira.Event.Type (TimestampedEvent (..), ViraEvent (..))
import Prelude hiding (Reader)

-- | Start a background thread that monitors and logs events
startEventLogger :: (Reader AppState :> es, IOE :> es) => Eff es ()
startEventLogger = do
  -- Subscribe to events with a buffer of 50 events
  loggerBuffer <- Broadcast.subscribe 50
  liftIO $ do
    putStrLn "🔔 Event logger started and subscribed"
    void $ async $ infinitely $ do
      -- Try to drain events from our logger buffer
      maybeEvents <- STM.atomically $ CB.drain loggerBuffer
      whenJust maybeEvents (mapM_ logEvent)
      threadDelay 1_000_000 -- Check every 1 second

-- | Log a single event to stdout
logEvent :: TimestampedEvent -> IO ()
logEvent (TimestampedEvent time event) = do
  let timeStr = formatTime defaultTimeLocale "%H:%M:%S" time
  putStrLn $ "🔔 [" <> timeStr <> "] " <> eventDescription event

-- | Get a human-readable description of an event
eventDescription :: ViraEvent -> String
eventDescription (ViraEvent eventType eventData) =
  "Action: " <> eventType <> " (data: " <> show (ByteString.take 20 eventData) <> "...)"
