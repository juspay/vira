module Vira.Lib.Sort where

-- | Remove duplicates and sort the list through the mapped list on the given function
uniqueSortOn :: (Ord k) => (a -> k) -> [a] -> [a]
uniqueSortOn chg = sortOn chg . ordNubOn chg
