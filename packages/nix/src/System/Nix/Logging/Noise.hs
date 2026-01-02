{-# LANGUAGE OverloadedStrings #-}

{- | Filtering and grouping of Nix override-input noise in logs

Nix outputs "Added input" and "Updated input" messages with multi-line
continuations when using --override-input. This module groups those lines
into collapsible blocks for cleaner log display.
-}
module System.Nix.Logging.Noise (
  -- * Grouping
  groupNixNoiseLines,
  LineGroup (..),
) where

import Data.Text qualified as T

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
