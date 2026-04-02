module RotationalCipher (rotate) where

import Data.Char (chr, isLower, isUpper, ord)

rotate :: Int -> String -> String
rotate key = map rot
  where
    rot c
        | isLower c = rot2 'z'
        | isUpper c = rot2 'Z'
        | otherwise = c
      where
        c1 = ord c + key
        rot2 z = chr $ if c1 > ord z then c1 - 26 else c1
