{-# OPTIONS -Wno-missing-signatures #-}
{-# LANGUAGE ViewPatterns #-}

module PigLatin (translate) where

import Data.List (isPrefixOf)

translate = unwords . map ((++ "ay") . pig) . words

pig word = case word of
    _ | any (`isPrefixOf` word) $ "xr" : "yt" : map pure "aeiou" -> word
    'q' : 'u' : b -> b ++ "qu"
    a : 'q' : 'u' : b -> b ++ a : "qu"
    a : (break (`elem` "aeiouy") -> (b, c)) -> c ++ a : b
    "" -> error "impossible"
