{-# LANGUAGE LambdaCase #-}

module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, isUpper, toUpper)
import Data.Maybe (listToMaybe, mapMaybe)

abbreviate :: String -> String
abbreviate = map toUpper . mapMaybe listToMaybe . splitWords
  where

    splitWords = \case
        []                                  -> []
        x : y : zs  | isLower x, isUpper y  -> [x] : splitWords (y : zs)
        x : ys      | isAlpha x             -> overHead (x :) $ splitInside ys
                    | otherwise             -> splitWords ys

    splitInside = \case
        []                  -> [[]]
        '\'' : zs           -> splitInside zs
        y : zs  | isAlpha y -> splitWords (y : zs)
                | otherwise -> [] : splitWords zs

    overHead f = \case [] -> []; x : xs  -> f x : xs
