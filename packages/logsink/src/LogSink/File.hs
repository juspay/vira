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
import System.IO (hClose, openFile)

{- | Create a sink that appends 'Text' lines to a file

Each call to 'sinkWrite' appends a line (with newline) to the file.
The file is opened in append mode and closed when 'sinkClose' is called.
-}
fileSink :: FilePath -> IO (Sink Text)
fileSink = fileSinkWith id

{- | Create a sink with custom encoder

@fileSinkWith encoder path@ creates a sink that encodes values using
@encoder@ before appending to the file.
-}
fileSinkWith :: (a -> Text) -> FilePath -> IO (Sink a)
fileSinkWith encoder path = do
  h <- openFile path AppendMode
  pure $ contramap encoder (handleSink h)

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
