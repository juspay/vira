{- |
Circular buffer implementation using STM TBMQueue with drop-oldest semantics.

This module provides a circular buffer built on top of "Control.Concurrent.STM.TBMQueue"
that automatically drops the oldest items when the buffer becomes full.

Example usage:

@
import Control.Concurrent.STM.CircularBuffer qualified as CB

main :: IO ()
main = atomically $ do
  buf <- CB.'new' 3
  CB.'add' "first" buf
  CB.'add' "second" buf
  CB.'add' "third" buf
  CB.'add' "fourth" buf  -- Drops "first"
  items <- CB.'drain' buf
  -- items will be Just ("second" :| ["third", "fourth"])
@
-}
module Control.Concurrent.STM.CircularBuffer (
  CircularBuffer,
  new,
  add,
  clone,
  drain,
  close,
) where

import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, isFullTBMQueue, newTBMQueue, readTBMQueue, tryReadTBMQueue, writeTBMQueue)

-- | Circular buffer using TBMQueue - when full, oldest items are dropped
data CircularBuffer a = CircularBuffer (TBMQueue a) Int

{- | Create a new circular buffer with the given capacity.

The buffer will hold at most @capacity@ items. When full, adding new items
will cause the oldest items to be dropped.
-}
new :: Int -> STM (CircularBuffer a)
new cap = CircularBuffer <$> newTBMQueue cap <*> pure cap

{- | Add an element to the circular buffer.

If the buffer is full, the oldest item will be dropped to make room
for the new item.
-}
add :: a -> CircularBuffer a -> STM ()
add item (CircularBuffer queue _) = do
  whenM (isFullTBMQueue queue) $ do
    void $ readTBMQueue queue -- Drop oldest item
  writeTBMQueue queue item

{- | Clone the contents of a circular buffer into a new buffer with the same capacity.

This operation drains all items from the source buffer, copies them to a new buffer,
and then puts them back into the source buffer. The items maintain their order.

Throws an error if the source buffer is closed.
-}
clone :: (HasCallStack) => CircularBuffer a -> STM (CircularBuffer a)
clone sourceBuffer@(CircularBuffer _ capacity) = do
  newBuffer <- new capacity
  -- Use drain to get all items without blocking if empty
  drain sourceBuffer >>= \case
    Nothing -> error "Cannot clone a closed CircularBuffer"
    Just (toList -> items) -> do
      -- Add all items to new buffer and put them back in source
      mapM_ (`add` newBuffer) items
      -- Put items back in source buffer (FIFO order)
      mapM_ (`add` sourceBuffer) items
      pure newBuffer

{- | Read all currently available items, blocking for the first item.

This function will block until at least one item is available, then drain
all remaining available items without blocking.

Returns 'Nothing' if the queue is closed, or @'Just' items@ if at least
one item is available.
-}
drain :: CircularBuffer a -> STM (Maybe (NonEmpty a))
drain (CircularBuffer queue _) = do
  -- First, block for one item (same behavior as readTBMQueue)
  readTBMQueue queue >>= \case
    Nothing -> pure Nothing -- Queue is closed
    Just x -> do
      -- Got first item, now collect all other available items without blocking
      let go acc = do
            item <- tryReadTBMQueue queue
            case item of
              Nothing -> pure (reverse acc) -- Queue is closed
              Just Nothing -> pure (reverse acc) -- No more items available
              Just (Just y) -> go (y : acc) -- Got another item
      rest <- go []
      pure $ Just (x :| rest)

{- | Close the buffer to signal end-of-stream.

Once closed, no more items can be added to the buffer, and 'drain'
will return 'Nothing' after all remaining items have been drained.
-}
close :: CircularBuffer a -> STM ()
close (CircularBuffer queue _) = closeTBMQueue queue
