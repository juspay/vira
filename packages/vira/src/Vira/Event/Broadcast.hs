-- | Event broadcasting system for multiple subscribers
module Vira.Event.Broadcast (
  subscribe,
  broadcastEvent,
) where

import Control.Concurrent.STM.CircularBuffer (CircularBuffer)
import Control.Concurrent.STM.CircularBuffer qualified as CB
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic (Reader, asks)
import Vira.App.Stack (AppState (..), eventSubscribers)
import Vira.Event.Type (TimestampedEvent (..), ViraEvent)
import Prelude hiding (Reader, asks)

-- | Subscribe to events by creating a new CircularBuffer
subscribe :: (Reader AppState :> es, IOE :> es) => Int -> Eff es (CircularBuffer TimestampedEvent)
subscribe bufferSize = do
  subscribers <- asks eventSubscribers
  liftIO $ atomically $ do
    newBuffer <- CB.new bufferSize
    currentSubs <- readTVar subscribers
    writeTVar subscribers (newBuffer : currentSubs)
    pure newBuffer

-- | Broadcast an event to all subscribers
broadcastEvent :: (Reader AppState :> es, IOE :> es) => ViraEvent -> Eff es ()
broadcastEvent event = do
  subscribers <- asks eventSubscribers
  now <- liftIO getCurrentTime
  let timestampedEvent = TimestampedEvent now event
  liftIO $ atomically $ do
    currentSubs <- readTVar subscribers
    mapM_ (CB.add timestampedEvent) currentSubs
