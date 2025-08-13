{-# LANGUAGE KindSignatures #-}

{- | A type-safe bounded circular buffer with compile-time capacity checking.

= Usage Example

@
buffer = new @100                            -- CircularBuffer 100 Text
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

{- | A circular buffer with type-level capacity that automatically trims old elements
The capacity @n@ is enforced at the type level for compile-time safety
-}
newtype CircularBuffer (n :: Nat) a = CircularBuffer (Seq.Seq a)
  deriving stock (Show, Eq, Foldable)

-- | Create a new empty circular buffer with type-level capacity
new :: forall n a. (KnownNat n) => CircularBuffer n a
new = CircularBuffer Seq.empty

-- | Add elements to the buffer, automatically trimming if capacity is exceeded
append :: forall n a. (KnownNat n) => [a] -> CircularBuffer n a -> CircularBuffer n a
append newItems (CircularBuffer buffer) =
  let capacity = fromIntegral $ natVal (Proxy @n)
      combined = buffer <> Seq.fromList newItems
      len = Seq.length combined
   in CircularBuffer $
        if len <= capacity
          then combined
          else Seq.drop (len - capacity) combined

-- | Check if buffer is empty
isEmpty :: CircularBuffer n a -> Bool
isEmpty (CircularBuffer buffer) = Seq.null buffer

-- | Get current number of elements in buffer
size :: CircularBuffer n a -> Int
size (CircularBuffer buffer) = Seq.length buffer
