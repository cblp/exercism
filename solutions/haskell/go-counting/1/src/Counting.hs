{-# LANGUAGE LambdaCase #-}

module Counting (
    Color (..),
    territories,
    territoryFor,
) where

import Control.Monad ((>=>))
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

data Color = Black | White deriving (Eq, Ord, Show)

type Coord = (Int, Int)

getCell :: [String] -> Coord -> Maybe Char
getCell board (x, y)
    | y < 1 || y > length board = Nothing
    | x < 1 || x > length row = Nothing
    | otherwise = Just $ row !! (x - 1)
  where
    row = board !! (y - 1)

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

charToColor :: Char -> Maybe Color
charToColor = \case
    'B' -> Just Black
    'W' -> Just White
    _ -> Nothing

floodFill :: [String] -> Coord -> Maybe (Set Coord)
floodFill board start =
    case getCell board start of
        Just ' ' -> Just $ go (Set.singleton start) [start]
        _ -> Nothing
  where
    go visited [] = visited
    go visited (c : queue) =
        let emptyNeighbors =
                [ n
                | n <- neighbors c
                , getCell board n == Just ' '
                , n `notElem` visited
                ]
        in  go (visited <>+ emptyNeighbors) (queue <> emptyNeighbors)

(<>+) :: (Foldable f, Ord a) => Set a -> f a -> Set a
(<>+) = foldr Set.insert

owner :: [String] -> Set Coord -> Maybe Color
owner board territory =
    the
        . Set.fromList
        $ foldMap (mapMaybe (getCell board >=> charToColor) . neighbors) territory

the :: Set a -> Maybe a
the s
    | length s == 1 = Just $ head $ toList s
    | otherwise = Nothing

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
    let rows = length board
        cols = if null board then 0 else maximum (map length board)
        allCoords = [(x, y) | y <- [1 .. rows], x <- [1 .. cols]]
        emptyCells = filter (\c -> getCell board c == Just ' ') allCoords
        go [] _ = []
        go (c : cs) visited
            | Set.member c visited = go cs visited
            | otherwise =
                case floodFill board c of
                    Nothing -> go cs visited
                    Just t -> (t, owner board t) : go cs (Set.union visited t)
    in  go emptyCells Set.empty

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
    case floodFill board coord of
        Just t -> Just (t, owner board t)
        Nothing -> Nothing
