{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Filtering and grouping of Nix override-input noise in logs

Nix outputs "Added input" and "Updated input" messages with multi-line
continuations when using --override-input. This module groups those lines
into collapsible blocks for cleaner log display.
-}
module LogSink.Contrib.NixNoise (
  -- * Grouping
  groupNixNoiseLines,
  LineGroup (..),

  -- * Detection predicates
  isNoiseStart,
  isNoiseContinuation,

  -- * Noise type encoding/decoding
  NixNoise (..),
  encodeNixNoise,
  decodeNixNoise,

  -- * Sink wrapper
  noiseGroupingSink,
) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Text qualified as T
import LogSink (Sink (..))

-- | A group of lines, either regular or Nix noise block
data LineGroup
  = RegularLine Text
  | NixNoiseBlock [Text]
  deriving stock (Show, Eq)

-- | Check if a line starts a Nix input noise block
isNoiseStart :: Text -> Bool
isNoiseStart line =
  "• Added input" `T.isPrefixOf` line
    || "• Updated input" `T.isPrefixOf` line

{- | Check if a line is a continuation of a noise block
  Only valid when already inside a noise block!
-}
isNoiseContinuation :: Text -> Bool
isNoiseContinuation line =
  "    " `T.isPrefixOf` line -- 4-space indented continuation
    || "  → " `T.isPrefixOf` line -- Arrow continuation

{- | Group consecutive noise lines into collapsible blocks

Only lines starting with "Added input" or "Updated input" start a noise block.
Indented continuations are only grouped when immediately following such a line.
-}
groupNixNoiseLines :: [Text] -> [LineGroup]
groupNixNoiseLines = reverse . finalize . foldl' groupStep (Nothing, [])
  where
    groupStep :: (Maybe [Text], [LineGroup]) -> Text -> (Maybe [Text], [LineGroup])
    groupStep (Nothing, acc) line
      | isNoiseStart line = (Just [line], acc) -- Start new noise block
      | otherwise = (Nothing, RegularLine line : acc) -- Regular line
    groupStep (Just noiseAcc, acc) line
      | isNoiseStart line = (Just (line : noiseAcc), acc) -- Another noise start, continue
      | isNoiseContinuation line = (Just (line : noiseAcc), acc) -- Continuation of noise
      | otherwise = (Nothing, RegularLine line : NixNoiseBlock (reverse noiseAcc) : acc) -- End block

    -- Finalize any remaining noise block
    finalize :: (Maybe [Text], [LineGroup]) -> [LineGroup]
    finalize (Nothing, acc) = acc
    finalize (Just noiseAcc, acc) = NixNoiseBlock (reverse noiseAcc) : acc

-- | Noise block data for encoding/decoding
newtype NixNoise = NixNoise
  { noiseLines :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Encode a NixNoise block to JSON Text
encodeNixNoise :: NixNoise -> Text
encodeNixNoise = decodeUtf8 . encode

-- | Decode a JSON Text to NixNoise, returns Nothing if not a noise block
decodeNixNoise :: Text -> Maybe NixNoise
decodeNixNoise = decode . encodeUtf8

{- | Wrap a sink with Nix noise grouping

Buffers noise lines and emits them as a single JSON entry when non-noise
arrives or on close. Uses 'NixNoise' JSON format.

The encoder function transforms NixNoise JSON into the final log format.
For ViraLog integration, pass a function that wraps the noise JSON appropriately.
-}
noiseGroupingSink ::
  -- | Encoder: transforms NixNoise JSON to final log line format
  (Text -> Text) ->
  -- | Underlying sink to wrap
  Sink Text ->
  IO (Sink Text)
noiseGroupingSink encodeNoise inner = do
  bufferRef <- newIORef ([] :: [Text])
  pure
    Sink
      { sinkWrite = \line -> do
          buffer <- readIORef bufferRef
          if
            | isNoiseStart line -> do
                -- Add to noise buffer
                modifyIORef' bufferRef (line :)
            | isNoiseContinuation line && not (null buffer) -> do
                -- Continue buffering
                modifyIORef' bufferRef (line :)
            | otherwise -> do
                -- Flush buffer if any, then write regular line
                flushNoiseBuffer bufferRef encodeNoise inner
                sinkWrite inner line
      , sinkFlush = do
          flushNoiseBuffer bufferRef encodeNoise inner
          sinkFlush inner
      , sinkClose = do
          flushNoiseBuffer bufferRef encodeNoise inner
          sinkClose inner
      }

-- | Flush accumulated noise buffer as a single JSON entry
flushNoiseBuffer ::
  IORef [Text] ->
  (Text -> Text) ->
  Sink Text ->
  IO ()
flushNoiseBuffer bufferRef encodeNoise sink = do
  buffer <- readIORef bufferRef
  unless (null buffer) $ do
    let noiseText = T.intercalate "\n" (reverse buffer)
        noiseJson = encodeNixNoise (NixNoise noiseText)
        logLine = encodeNoise noiseJson
    sinkWrite sink logLine
    writeIORef bufferRef []
