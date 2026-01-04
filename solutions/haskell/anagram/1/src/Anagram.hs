{-# LANGUAGE ViewPatterns #-}

module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor w = filter isAnagram
  where
    wl = map toLower w
    ws = sort wl
    isAnagram (map toLower -> cl) = cl /= wl && sort cl == ws
