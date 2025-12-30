module DNA (toRNA) where

import Data.Function ((&))

toRNA :: String -> Either Char String
toRNA = traverse charToRna

charToRna :: Char -> Either Char Char
charToRna c =
    lookup c [('G', 'C'), ('C', 'G'), ('T', 'A'), ('A', 'U')]
        & maybe (Left c) Right
