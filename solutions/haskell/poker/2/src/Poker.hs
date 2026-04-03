{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Poker (bestHands) where

import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Functor (($>))
import Data.List (nub, sortOn)
import Data.Map qualified as Map
import Data.Ord (Down (Down))

bestHands :: [String] -> Maybe [String]
bestHands handsStr = do
    hands <- traverse parseHand handsStr
    Just $ map snd $ maximumsOn (score . fst) $ zip hands handsStr

data Scoring
    = Values [Int]
    | OnePair Int
    | TwoPairs {firstPair, secondPair, kicker :: Int}
    | ThreeOfAKind {value, remaining1, remaining2 :: Int}
    | Straight Int
    | Flush [Int]
    | FullHouse {triplet :: Int, pair :: Int}
    | FourOfAKind {value :: Int, remaining :: Int}
    | StraightFlush Int
    deriving (Eq, Ord, Show)

parseHand :: String -> Maybe [(Int, Char)]
parseHand = traverse parseCard . words

parseCard :: String -> Maybe (Int, Char)
parseCard = \case
    ['A', s] -> Just (14, s)
    ['K', s] -> Just (13, s)
    ['Q', s] -> Just (12, s)
    ['J', s] -> Just (11, s)
    ['1', '0', s] -> Just (10, s)
    [v, s] -> Just (digitToInt v, s)
    _ -> Nothing

score :: [(Int, Char)] -> Scoring
score cards =
    parseStraight
        ?: (guard isFlush $> Flush valuesDown)
        ?: case valuesByFrequencyDown of
            [(4, value), (1, remaining)] -> FourOfAKind{..}
            [(3, triplet), (2, pair)] -> FullHouse{..}
            [(3, value), (1, remaining1), (1, remaining2)] -> ThreeOfAKind{..}
            [(2, firstPair), (2, secondPair), (1, kicker)] -> TwoPairs{..}
            (2, v) : _ -> OnePair v
            _ -> Values valuesDown
  where
    -- values, sorted down
    valuesDown = sortOn Down $ map fst cards

    -- cards, grouped by value
    groupedByValue = Map.fromListWith (++) [(v, [s]) | (v, s) <- cards]

    -- values, grouped by frequency, then sorted by frequency down
    valuesByFrequencyDown =
        sortOn Down [(length ss, v) | (v, ss) <- Map.assocs groupedByValue]

    parseStraight =
        mkStraight
            <$> case valuesDown of
                -- ace can start a straight
                [14, 5, 4, 3, 2] -> Just 5
                _ | isConsecutiveDown valuesDown -> Just $ head valuesDown
                _ -> Nothing

    mkStraight | isFlush = StraightFlush | otherwise = Straight

    isFlush = length (nub $ map snd cards) == 1

maximumsOn :: (Ord b) => (a -> b) -> [a] -> [a]
maximumsOn _ [] = []
maximumsOn k (x : xs) = go (k x) [x] xs
  where
    go _ acc [] = acc
    go m acc (y : ys)
        | ky == m = go m (y : acc) ys
        | ky > m = go ky [y] ys
        | otherwise = go m acc ys
      where
        ky = k y

(?:) :: Maybe a -> a -> a
Nothing ?: x = x
Just x ?: _ = x
infixr 0 ?:

isConsecutiveDown :: (Enum a, Eq a) => [a] -> Bool
isConsecutiveDown = \case
    [] -> True
    x : xs -> isConsecutiveDown' x xs
  where
    isConsecutiveDown' x = \case
        [] -> True
        y : ys -> y == pred x && isConsecutiveDown' y ys
