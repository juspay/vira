{- | Module for defining sort utilities

If more requirements arise, prefer using https://hackage.haskell.org/package/sort instead
-}
module Vira.Lib.Sort where

-- | Remove duplicates and sort the list through the mapped list on the given function
uniqueSortOn :: (Ord k) => (a -> k) -> [a] -> [a]
uniqueSortOn chg = sortOn chg . ordNubOn chg
