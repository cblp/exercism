module Grains (square, total) where

import Data.Bits
import Data.Word

square :: Integer -> Maybe Integer
square n = (1 <= n && n <= 64) `thenPure` bit (fromInteger n - 1)
  where
    thenPure c x = if c then Just x else Nothing

total :: Integer
total = toInteger (maxBound :: Word64)
