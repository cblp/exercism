{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ParallelListComp #-}

module WordSearch (search, CharPos (..), WordPos (..)) where

import Prelude hiding (last, lines, seq)

import Data.Foldable (toList)
import Data.List (findIndex, isPrefixOf, tails)
import Data.Maybe (listToMaybe)

type Grid = [[Char]]

data CharPos = CharPos {row :: !Int, col :: !Int} deriving (Eq, Show)

data WordPos = WordPos {start :: !CharPos, end :: !CharPos} deriving (Eq, Show)

-- | p + q * i
addMul :: CharPos -> CharPos -> Int -> CharPos
addMul p q i = CharPos{col = p.col + q.col * i, row = p.row + q.row * i}

base1 :: CharPos -> CharPos
base1 pos = CharPos{col = pos.col + 1, row = pos.row + 1}

data Line = Line
    { letters :: String
    , start :: CharPos
    , direction :: CharPos
    -- ^ (-1 | 0 | +1) for each
    }
    deriving (Eq, Show)

search :: Grid -> [String] -> [(String, Maybe WordPos)]
search grid wordList = [(word, search' grid word) | word <- wordList]

search' :: Grid -> String -> Maybe WordPos
search' grid word =
    listToMaybe
        [ WordPos
            { start = base1 $ addMul line.start line.direction offset
            , end =
                base1 $
                    addMul line.start line.direction (offset + length word - 1)
            }
        | line <- lines grid
        , offset <- toList $ findInfix word line.letters
        ]

findInfix :: (Eq a) => [a] -> [a] -> Maybe Int
findInfix needle haystack = findIndex (needle `isPrefixOf`) (tails haystack)

lines :: Grid -> [Line]
lines grid =
    concat
        [ (↖) ++ (↑) ++ (↗)
        , (⇐) {-  -} ++ (→)
        , (↙) ++ (↓) ++ (↘)
        ]
  where
    height = length grid
    width = length $ head grid

    line (dr, dc) (sr, sc) =
        Line
            { letters =
                [ grid !! r !! c
                | r <- seq sr dr (height - 1)
                | c <- seq sc dc (width - 1)
                ]
            , start = CharPos{col = sc, row = sr}
            , direction = CharPos{col = dc, row = dr}
            }

    seq s d last =
        case compare d 0 of
            LT -> [s, s - 1 .. 0]
            EQ -> repeat s
            GT -> [s, s + 1 .. last]

    (→) = [line (0, 1) (r, 0) | r <- [0 .. height - 1]]

    (⇐) = [line (0, -1) (r, width - 1) | r <- [0 .. height - 1]]

    (↓) = [line (1, 0) (0, c) | c <- [0 .. width - 1]]

    (↑) = [line (-1, 0) (height - 1, c) | c <- [0 .. width - 1]]

    (↘) =
        [line (1, 1) (0, c) | c <- [0 .. width - 1]]
            ++ [line (1, 1) (r, 0) | r <- [1 .. height - 1]]

    (↖) =
        [line (-1, -1) (height - 1, c) | c <- [0 .. width - 1]]
            ++ [line (-1, -1) (r, width - 1) | r <- [0 .. height - 2]]

    (↙) =
        [line (1, -1) (0, c) | c <- [0 .. width - 1]]
            ++ [line (1, -1) (r, width - 1) | r <- [1 .. height - 1]]

    (↗) =
        [line (-1, 1) (height - 1, c) | c <- [0 .. width - 1]]
            ++ [line (-1, 1) (r, 0) | r <- [0 .. height - 2]]
