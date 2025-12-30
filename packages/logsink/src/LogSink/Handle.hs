{- |
Module      : Control.Concurrent.Sink.Handle
Description : Drain process handles into sinks
Copyright   : (c) Sridhar Ratnakumar, 2025
License     : MIT
Maintainer  : srid@srid.ca

Utilities for draining process stdout/stderr handles into sinks.
This is essential for capturing subprocess output in a structured way.
-}
module LogSink.Handle (
  -- * Handle drainage
  drainHandle,
  drainHandleWith,
) where

import LogSink (Sink (..))
import System.IO (hGetLine)

{- | Drain all lines from a 'Handle' into a 'Sink'

Reads lines until EOF, writing each to the sink.
Does not close the handle when done.

This function blocks until EOF is reached. For concurrent
drainage of multiple handles, use 'Control.Concurrent.Async.withAsync'.
-}
drainHandle :: Handle -> Sink Text -> IO ()
drainHandle = drainHandleWith id

{- | Drain handle with transformation per line

@drainHandleWith f h sink@ reads lines from @h@, transforms each
with @f@, and writes to @sink@.
-}
drainHandleWith :: (Text -> a) -> Handle -> Sink a -> IO ()
drainHandleWith transform h sink = go
  where
    go = do
      eof <- hIsEOF h
      if eof
        then pass
        else do
          -- Read line as String, convert to Text
          line <- toText <$> hGetLine h
          sinkWrite sink (transform line)
          go
