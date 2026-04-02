module Transpose (transpose) where

import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)

transpose :: [String] -> [String]
transpose rows = [map (fromMaybe ' ') $ rstrip col | col <- zipLongest rows]

zipLongest :: [[Char]] -> [[Maybe Char]]
zipLongest = go
  where
    go [] = []
    go xss
        | all null xss = []
        | otherwise = map listToMaybe xss : go (map (drop 1) xss)

rstrip :: [Maybe a] -> [Maybe a]
rstrip = dropWhileEnd isNothing
