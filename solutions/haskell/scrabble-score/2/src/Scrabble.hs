{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Scrabble (scoreLetter, scoreWord) where

import Prelude

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter
    | "AEIOULNRST" = 1
    | "DG" = 2
    | "BCMP" = 3
    | "FHVWY" = 4
    | "K" = 5
    | "JX" = 8
    | "QZ" = 10
    | otherwise = 0
  where
    fromString s = toUpper letter `elem` s

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
