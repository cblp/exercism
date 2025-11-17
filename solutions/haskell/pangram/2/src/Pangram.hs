module Pangram (isPangram) where

import Data.Char (isAsciiLower, toLower)
import Data.Set (fromList, isSubsetOf)

isPangram :: String -> Bool
isPangram =
    (alphabet `isSubsetOf`) . fromList . filter isAsciiLower . map toLower
  where
    alphabet = fromList ['a' .. 'z']
