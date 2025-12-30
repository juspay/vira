{-# LANGUAGE RankNTypes #-}

{- |
Module      : Control.Concurrent.Sink
Description : Abstract sink for concurrent writes
Copyright   : (c) Sridhar Ratnakumar, 2025
License     : MIT
Maintainer  : srid@srid.ca

A 'Sink' is an abstraction for writing values to some destination.
Sinks are:

  * 'Contravariant' - transform input before writing
  * 'Semigroup' - combine sinks to write to multiple destinations
  * 'Monoid' - empty sink that discards all input

Example usage:

@
-- Create a file sink
fileSink <- fileSinkWith encodeJSON "output.jsonl"

-- Create a broadcast for streaming
broadcast <- newBroadcast 1000
let streamSink = broadcastSink broadcast

-- Fan-out: write to both
let sink = fileSink <> streamSink

-- Transform input type
let logEntrySink = contramap encodeLogEntry textSink

-- Write log entries
sinkWrite sink logEntry
@
-}
module LogSink (
  -- * Core type
  Sink (..),

  -- * Constructors
  nullSink,
) where

-- | A sink that accepts values of type @a@
data Sink a = Sink
  { sinkWrite :: a -> IO ()
  -- ^ Write a single value to the sink
  , sinkFlush :: IO ()
  -- ^ Flush any buffered writes
  , sinkClose :: IO ()
  -- ^ Close the sink and release resources
  }

-- | Sink that discards all input
nullSink :: Sink a
nullSink = Sink (const pass) pass pass

-- | Transform input before writing
instance Contravariant Sink where
  contramap f (Sink w fl cl) = Sink (w . f) fl cl

-- | Combine sinks: write to both
instance Semigroup (Sink a) where
  Sink w1 f1 c1 <> Sink w2 f2 c2 =
    Sink
      (\a -> w1 a >> w2 a)
      (f1 >> f2)
      (c1 >> c2)

-- | Empty sink that discards all input
instance Monoid (Sink a) where
  mempty = nullSink
