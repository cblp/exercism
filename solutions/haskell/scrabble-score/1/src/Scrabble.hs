module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

scores :: [(Char, Integer)]
scores =
    [ (letter, value)
    | (letters, value) <-
        [ ("AEIOULNRST", 1)
        , ("DG", 2)
        , ("BCMP", 3)
        , ("FHVWY", 4)
        , ("K", 5)
        , ("JX", 8)
        , ("QZ", 10)
        ]
    , letter <- letters
    ]

scoreLetter :: Char -> Integer
scoreLetter letter = fromMaybe 0 $ lookup (toUpper letter) scores

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
