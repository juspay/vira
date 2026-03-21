-- | STM utilities for concurrent programming
module Vira.Lib.STM (
  drainTChan,
  drainRemainingTChan,
) where

import Control.Concurrent.STM (TChan, readTChan, tryReadTChan)

{- | Drain all items from a 'TChan' (equivalent to @CB.drain@)

Blocks until at least one item is available, then drains all remaining items.
-}
drainTChan :: TChan a -> STM (NonEmpty a)
drainTChan chan = do
  first <- readTChan chan
  rest <- drainRemainingTChan chan
  pure $ first :| rest

-- | Drain remaining items from 'TChan' without blocking
drainRemainingTChan :: TChan a -> STM [a]
drainRemainingTChan chan = reverse <$> go []
  where
    go acc =
      tryReadTChan chan >>= \case
        Nothing -> pure acc
        Just item -> go (item : acc)
