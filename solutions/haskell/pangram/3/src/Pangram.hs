module Pangram (isPangram) where

import Data.Char (isAsciiLower, toLower)
import Data.Set (fromList)

isPangram :: String -> Bool
isPangram = (== 26) . length . fromList . filter isAsciiLower . map toLower
