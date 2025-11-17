module Pangram (isPangram) where

import Data.Char (isAsciiLower, toLower)
import Data.List (nub, sort)

isPangram :: String -> Bool
isPangram = (== ['a' .. 'z']) . nub . sort . filter isAsciiLower . map toLower
