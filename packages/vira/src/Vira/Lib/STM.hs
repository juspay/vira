-- | STM utilities for concurrent programming
module Vira.Lib.STM (
  drainTChan,
  drainRemainingTChan,
) where

import Control.Concurrent.STM (TChan, readTChan, tryReadTChan)
import Data.List.NonEmpty qualified as NonEmpty

{- | Drain all items from a 'TChan' (equivalent to @CB.drain@)

Blocks until at least one item is available, then drains all remaining items.
-}
drainTChan :: TChan a -> STM (NonEmpty a)
drainTChan chan = do
  -- Block until first item is available
  firstItem <- readTChan chan
  -- Then drain any remaining items without blocking
  remainingItems <- drainRemainingTChan chan
  pure $ NonEmpty.fromList (firstItem : remainingItems)

-- | Drain remaining items from 'TChan' without blocking
drainRemainingTChan :: TChan a -> STM [a]
drainRemainingTChan chan = do
  items <- drainLoop []
  pure $ reverse items
  where
    drainLoop acc = do
      maybeItem <- tryReadTChan chan
      case maybeItem of
        Nothing -> pure acc
        Just item -> drainLoop (item : acc)
