{- | A bounded circular buffer with runtime capacity checking.

= Usage Example

@
buffer = new 100                                -- CircularBuffer with capacity 100
buffer1 = append ["line1", "line2"] buffer
buffer2 = append ["line3", "line4"] buffer1          -- keeps last 100 lines
toList buffer2                              -- ["line1", "line2", "line3", "line4"]
@

= Performance

* 'append': O(k + log n) where k = new items, n = capacity
* 'toList': O(n) where n = current size
* 'isEmpty': O(1)
* 'size': O(1)
-}
module Vira.Lib.CircularBuffer (
  CircularBuffer,
  new,
  append,
  isEmpty,
  size,
) where

import Data.Sequence qualified as Seq

{- | A circular buffer with runtime capacity that automatically trims old elements
The capacity is stored as a runtime value for flexibility
-}
data CircularBuffer a = CircularBuffer
  { capacity :: Natural
  , buffer :: Seq.Seq a
  }
  deriving stock (Show, Eq)

-- | Create a new empty circular buffer with runtime capacity
new :: Natural -> CircularBuffer a
new cap = CircularBuffer cap Seq.empty

-- | Add elements to the buffer, automatically trimming if capacity is exceeded
append :: [a] -> CircularBuffer a -> CircularBuffer a
append newItems (CircularBuffer cap buf) =
  let combined = buf <> Seq.fromList newItems
      len = Seq.length combined
      maxLen = fromIntegral cap
   in CircularBuffer cap $
        if len <= maxLen
          then combined
          else Seq.drop (len - maxLen) combined

-- | Check if buffer is empty
isEmpty :: CircularBuffer a -> Bool
isEmpty (CircularBuffer _ buf) = Seq.null buf

-- | Get current number of elements in buffer
size :: CircularBuffer a -> Int
size (CircularBuffer _ buf) = Seq.length buf

-- | Implement Foldable for CircularBuffer
instance Foldable CircularBuffer where
  foldMap f (CircularBuffer _ buf) = foldMap f buf
