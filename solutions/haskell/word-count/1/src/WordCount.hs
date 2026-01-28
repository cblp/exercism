{-# LANGUAGE ViewPatterns #-}

module WordCount (wordCount) where

import Control.Arrow ((>>>))
import Data.Char (isDigit, isLetter, toLower)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map

wordCount :: String -> [(String, Int)]
wordCount =
    splitWords
        >>> map ((,1) . map toLower . dropWhileEnd (== '\''))
        >>> Map.fromListWith (+)
        >>> Map.assocs

splitWords :: [Char] -> [String]
splitWords s =
    case dropWhile isNonWordy s of
        [] -> []
        '\'' : xs -> splitWords xs
        x : (break isNonWordy -> (w, z)) -> (x : w) : splitWords (drop 1 z)
  where
    isNonWordy c = not $ isLetter c || isDigit c || c == '\''
