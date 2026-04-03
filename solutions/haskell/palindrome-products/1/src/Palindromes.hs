{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (foldl')
import Data.Set (Set)
import Data.Set qualified as Set

data Palindrome = Palindrome
    { value :: Integer
    , factors :: Set (Integer, Integer)
    }
    deriving (Eq, Show)

data MinMax = Min | Max

largestPalindrome ::
    Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome = getPalindrome Max

smallestPalindrome ::
    Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome = getPalindrome Min

getPalindrome ::
    MinMax -> Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
getPalindrome chooser minFactor maxFactor =
    fromPalindrome
        <$> foldl'
            (choose chooser)
            Nothing
            [ Palindrome{value, factors = Set.singleton (a, b)}
            | a <- [minFactor .. maxFactor]
            , b <- [a .. maxFactor]
            , let value = a * b
            , isPalindrome value
            ]

isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

choose :: MinMax -> Maybe Palindrome -> Palindrome -> Maybe Palindrome
choose _ Nothing p = Just p
choose chooser (Just p1) p2 =
    Just
        case (compare p1.value p2.value, chooser) of
            (EQ, _) -> p1{factors = p1.factors <> p2.factors}
            (LT, Max) -> p2
            (GT, Min) -> p2
            _ -> p1

fromPalindrome :: Palindrome -> (Integer, [(Integer, Integer)])
fromPalindrome Palindrome{value, factors} = (value, Set.toList factors)
