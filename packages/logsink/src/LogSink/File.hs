{- |
Module      : Control.Concurrent.Sink.File
Description : File-backed sink with atomic line writes
Copyright   : (c) Sridhar Ratnakumar, 2025
License     : MIT
Maintainer  : srid@srid.ca

File sinks append lines atomically to a file. Each write appends a single
line (with newline) to the file.
-}
module LogSink.File (
  -- * File sinks
  fileSink,
  fileSinkWith,
  handleSink,
) where

import Data.Text.IO qualified as TextIO
import LogSink (Sink (..))
import System.IO (hClose)

{- | Create a sink that appends 'Text' lines to a file

Each call to 'sinkWrite' opens the file, appends a line (with newline),
and closes the file. This allows other processes to read the file
between writes without file locking issues.
-}
fileSink :: FilePath -> IO (Sink Text)
fileSink = fileSinkWith id

{- | Create a sink with custom encoder

@fileSinkWith encoder path@ creates a sink that encodes values using
@encoder@ before appending to the file.

Each write opens/appends/closes to avoid holding file locks.
-}
fileSinkWith :: (a -> Text) -> FilePath -> IO (Sink a)
fileSinkWith encoder path = do
  -- Touch the file to ensure it exists
  writeFile path ""
  pure $
    Sink
      { sinkWrite = \a -> withFile path AppendMode $ \h -> do
          TextIO.hPutStrLn h (encoder a)
          hFlush h
      , sinkFlush = pass -- No-op since each write is self-contained
      , sinkClose = pass -- No handle to close
      }

{- | Create a sink that writes to an existing 'Handle'

Each write appends a line with newline. The handle is closed
when 'sinkClose' is called.
-}
handleSink :: Handle -> Sink Text
handleSink h =
  Sink
    { sinkWrite = \t -> TextIO.hPutStrLn h t >> hFlush h
    , sinkFlush = hFlush h
    , sinkClose = hClose h
    }
