module BinarySearch (find) where

import Data.Array

find :: (Ord a) => Array Int a -> a -> Maybe Int
find arr x = go 0 (length arr - 1)
  where
    go low high =
        case compare x (arr ! mid) of
            _ | low > high -> Nothing
            LT -> go low (mid - 1)
            EQ -> Just mid
            GT -> go (mid + 1) high
      where
        mid = low + (high - low) `div` 2
