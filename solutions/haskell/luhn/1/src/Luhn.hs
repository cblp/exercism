module Luhn (isValid) where

import Data.Char (digitToInt, isDigit)

isValid :: String -> Bool
isValid cardNum = length digits >= 2 && checksum `mod` 10 == 0
  where
    digits = map digitToInt $ filter isDigit cardNum
    checksum = sum $ zipWith step [1 :: Int ..] $ reverse digits
    step n digit
        | odd n = digit
        | digitX2 > 9 = digitX2 - 9
        | otherwise = digitX2
      where
        digitX2 = digit * 2
