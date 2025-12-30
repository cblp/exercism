module SumOfMultiples (sumOfMultiples) where

import Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
    sum $ mconcat [Set.fromList [m, m + m .. limit - 1] | m <- factors, m > 0]
