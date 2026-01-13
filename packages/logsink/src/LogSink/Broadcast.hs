{- |
Module      : Control.Concurrent.Sink.Broadcast
Description : Pub/sub pattern with catchup ring buffer
Copyright   : (c) Sridhar Ratnakumar, 2025
License     : MIT
Maintainer  : srid@srid.ca

A 'Broadcast' is a pub/sub channel with a ring buffer for late subscribers.
When a subscriber joins, they receive the buffered history plus all new writes.

This is useful for log streaming where:

  * Multiple producers write concurrently
  * Late subscribers (e.g., web clients) need to catch up on recent history
  * The stream can be closed gracefully

Example:

@
broadcast <- newBroadcast 1000
let sink = broadcastSink broadcast

-- Producer writes
sinkWrite sink "line 1"
sinkWrite sink "line 2"

-- Subscriber joins and gets buffered lines + new ones
queue <- bcSubscribe broadcast
lines <- atomically $ CB.drain queue
-- lines = Just ["line 1", "line 2"]

-- Close signals end to all subscribers
bcClose broadcast
@
-}
module LogSink.Broadcast (
  -- * Broadcast type
  Broadcast (..),

  -- * Operations
  newBroadcast,
  broadcastSink,

  -- * Re-exports for subscribers
  CircularBuffer,
  drain,
) where

import Control.Concurrent.STM.CircularBuffer (CircularBuffer, drain)
import Control.Concurrent.STM.CircularBuffer qualified as CB
import LogSink (Sink (..))

-- | Broadcast channel with subscriber support
data Broadcast a = Broadcast
  { bcWrite :: a -> STM ()
  -- ^ Write a value to all subscribers (STM action)
  , bcSubscribe :: IO (CircularBuffer a)
  -- ^ Subscribe and get a queue with buffered history
  , bcClose :: IO ()
  -- ^ Close the broadcast, signaling end to all subscribers
  }

{- | Create a broadcast with the given ring buffer capacity

The capacity determines how many recent items late subscribers receive.
-}
newBroadcast :: Int -> IO (Broadcast a)
newBroadcast capacity = do
  ringBuffer <- atomically $ CB.new capacity
  queuesVar <- newTVarIO []
  pure
    Broadcast
      { bcWrite = \a -> do
          -- Add to ring buffer for late subscribers
          CB.add a ringBuffer
          -- Distribute to all current subscribers
          queues <- readTVar queuesVar
          forM_ queues $ \q -> CB.add a q
      , bcSubscribe = atomically $ do
          -- Clone ring buffer to get buffered history
          queue <- CB.clone ringBuffer
          -- Add to active subscribers
          modifyTVar' queuesVar (queue :)
          pure queue
      , bcClose = atomically $ do
          queues <- readTVar queuesVar
          forM_ queues CB.close
      }

{- | Convert a 'Broadcast' to a 'Sink'

This allows using a broadcast as a write destination alongside
other sinks (e.g., file sinks) via the 'Semigroup' instance.
-}
broadcastSink :: Broadcast a -> Sink a
broadcastSink bc =
  Sink
    { sinkWrite = atomically . bc.bcWrite
    , sinkFlush = pass
    , sinkClose = bc.bcClose
    }
