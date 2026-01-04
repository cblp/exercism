{-# LANGUAGE LambdaCase #-}

module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, isUpper, toUpper)
import Data.Maybe (catMaybes, listToMaybe)

abbreviate :: String -> String
abbreviate xs =
    map toUpper $ catMaybes [listToMaybe word | word <- splitWords xs]

splitWords :: String -> [String]
splitWords = \case
    []                                  -> []
    x : y : zs  | isLower x, isUpper y  -> [x] : splitWords (y : zs)
    x : ys      | isAlpha x             -> overHead (x :) $ insideWord ys
                | otherwise             -> splitWords ys
  where
    insideWord = \case
        []                  -> [[]]
        '\'' : zs           -> insideWord zs
        y : zs  | isAlpha y -> splitWords (y : zs)
                | otherwise -> [] : splitWords zs

overHead :: (a -> a) -> [a] -> [a]
overHead f = \case
    []      -> []
    x : xs  -> f x : xs
