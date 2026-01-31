module WordSearch (row, col, search, WordPos (..), CharPos (CharPos)) where

import Prelude hiding (last, lines, seq)

import Data.Ix (inRange, range)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)

type Grid = [[Char]]

data CharPos = CharPos {row :: !Int, col :: !Int} deriving (Eq, Show)

data WordPos = WordPos {start :: !CharPos, end :: !CharPos} deriving (Eq, Show)

search :: Grid -> [String] -> [(String, Maybe WordPos)]
search grid wordList = [(word, search1 word) | word <- wordList]
  where
    search1 word =
        listToMaybe
            [ WordPos (CharPos sr sc) (addMul start direction (length word - 1))
            | start@(sr, sc) <- range bounds
            , direction <- directions
            , word `isPrefixOf` ray direction start
            ]

    bounds = ((1, 1), (length grid, length $ head grid))

    add (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

    addMul (r, c) (dr, dc) i = CharPos (r + dr * i) (c + dc * i)

    ray d = map at . takeWhile (inRange bounds) . iterate (add d)

    at (r, c) = grid !! (r - 1) !! (c - 1)

    directions =
        [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
