module Pangram (isPangram) where

import Data.Char (isAsciiLower, toLower)
import Data.Set (delete, fromList)

isPangram :: String -> Bool
isPangram = (alphabet `isSubsetOf`) . filter isAsciiLower . map toLower
  where
    alphabet = fromList ['a' .. 'z']

    s `isSubsetOf` input =
        case input of
            _ | null s  -> True
            []          -> False
            x : xs      -> delete x s `isSubsetOf` xs
