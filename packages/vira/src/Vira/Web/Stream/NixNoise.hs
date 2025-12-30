{-# LANGUAGE OverloadedStrings #-}

-- | Filtering and grouping of Nix override-input noise in logs
module Vira.Web.Stream.NixNoise (
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

-- | Check if a line is Nix input noise that should be grouped
isNixInputNoise :: Text -> Bool
isNixInputNoise line =
  "• Added input" `T.isPrefixOf` line
    || "• Updated input" `T.isPrefixOf` line
    || "    " `T.isPrefixOf` line -- Indented continuation
    || "  → " `T.isPrefixOf` line -- Arrow continuation

-- | Group consecutive noise lines into collapsible blocks
groupNixNoiseLines :: [Text] -> [LineGroup]
groupNixNoiseLines = reverse . finalize . foldl' groupStep (Nothing, [])
  where
    groupStep :: (Maybe [Text], [LineGroup]) -> Text -> (Maybe [Text], [LineGroup])
    groupStep (Nothing, acc) line
      | isNixInputNoise line = (Just [line], acc) -- Start new noise block
      | otherwise = (Nothing, RegularLine line : acc) -- Regular line
    groupStep (Just noiseAcc, acc) line
      | isNixInputNoise line = (Just (line : noiseAcc), acc) -- Continue noise block
      | otherwise = (Nothing, RegularLine line : NixNoiseBlock (reverse noiseAcc) : acc) -- End block

    -- Finalize any remaining noise block
    finalize :: (Maybe [Text], [LineGroup]) -> [LineGroup]
    finalize (Nothing, acc) = acc
    finalize (Just noiseAcc, acc) = NixNoiseBlock (reverse noiseAcc) : acc
