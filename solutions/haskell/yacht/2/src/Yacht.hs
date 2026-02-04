{-# OPTIONS -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns #-}

module Yacht (yacht, Category (..)) where

import Data.List (sort)

data Category
    = Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht
    deriving (Enum, Show)

yacht :: Category -> [Int] -> Int
yacht category (sort -> dice@[a, b, c, d, e]) =
    case category of
        (succ . fromEnum -> x) | x <= 6 -> sum $ filter (x ==) dice
        FullHouse | a == b, b == c || c == d, b /= e, d == e -> sum dice
        FourOfAKind | countOf a >= 4 -> a * 4 | countOf b >= 4 -> b * 4
        LittleStraight | dice == [1 .. 5] -> 30
        BigStraight | dice == [2 .. 6] -> 30
        Choice -> sum dice
        Yacht | all (a ==) dice -> 50
        _ -> 0
  where
    countOf x = length $ filter (x ==) dice
