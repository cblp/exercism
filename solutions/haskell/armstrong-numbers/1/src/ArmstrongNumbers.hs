module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: Int -> Bool
armstrong a = a == sum [digitToInt d ^ n | d <- s]
  where
    s = show a
    n = length s
