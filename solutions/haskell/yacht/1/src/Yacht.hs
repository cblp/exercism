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
    deriving (Show)

yacht :: Category -> [Int] -> Int
yacht Ones dice = sum $ filter (1 ==) dice
yacht Twos dice = sum $ filter (2 ==) dice
yacht Threes dice = sum $ filter (3 ==) dice
yacht Fours dice = sum $ filter (4 ==) dice
yacht Fives dice = sum $ filter (5 ==) dice
yacht Sixes dice = sum $ filter (6 ==) dice
yacht FullHouse dice@(sort -> [a, b, c, d, e])
    | a == b, b == c || c == d, b /= e, d == e = sum dice
yacht FourOfAKind dice@(d1 : d2 : _)
    | countOf d1 dice >= 4 = d1 * 4
    | countOf d2 dice >= 4 = d2 * 4
yacht LittleStraight dice | sort dice == [1 .. 5] = 30
yacht BigStraight dice | sort dice == [2 .. 6] = 30
yacht Choice dice = sum dice
yacht Yacht (d1 : ds) | all (d1 ==) ds = 50
yacht _ _ = 0

countOf :: (Eq a) => a -> [a] -> Int
countOf x = length . filter (x ==)
