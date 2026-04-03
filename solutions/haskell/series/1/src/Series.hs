module Series (slices) where

import Data.Char
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n = filter ((n ==) . length) . map (take n) . tails . map digitToInt
