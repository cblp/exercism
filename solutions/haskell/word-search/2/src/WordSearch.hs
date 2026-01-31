{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module WordSearch (pattern CharPos, row, col, search, WordPos (..)) where

import Prelude hiding (last, lines, seq)

import Data.Ix (inRange, range)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)

type Grid = [[Char]]

type CharPos = (Int, Int)

pattern CharPos :: a -> b -> (a, b)
pattern CharPos{row, col} = (row, col)

data WordPos = WordPos {start :: !CharPos, end :: !CharPos} deriving (Eq, Show)

search :: Grid -> [String] -> [(String, Maybe WordPos)]
search grid wordList = [(word, search1 word) | word <- wordList]
  where
    search1 word =
        listToMaybe
            [ WordPos{start, end = addMul start direction (length word - 1)}
            | start <- range bounds
            , direction <- directions
            , word `isPrefixOf` ray direction start
            ]

    bounds = ((1, 1), (length grid, length $ head grid))

    add (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

    addMul (r, c) (dr, dc) i = (r + dr * i, c + dc * i)

    ray d = map at . takeWhile (inRange bounds) . iterate (add d)

    at (r, c) = grid !! (r - 1) !! (c - 1)

    directions =
        [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
