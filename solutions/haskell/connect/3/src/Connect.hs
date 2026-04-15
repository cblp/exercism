module Connect (Mark (..), winner) where

import Data.Foldable (toList)
import Data.Ix (inRange)
import Data.Set qualified as Set

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [[Char]] -> Maybe Mark
winner rawBoard
    | wins Cross = Just Cross
    | wins Nought = Just Nought
    | otherwise = Nothing
  where
    board = map (filter (/= ' ')) rawBoard
    h = length board
    w = case board of x : _ -> length x; _ -> 0

    at (i, j)
        | inRange ((0, 0), (h - 1, w - 1)) (i, j) = Just $ board !! i !! j
        | otherwise = Nothing

    wins mark = any isEnd reachable
      where
        ch = symbol mark
        starts = case mark of
            Cross -> [(i, 0) | i <- [0 .. h - 1], at (i, 0) == Just ch]
            Nought -> [(0, j) | j <- [0 .. w - 1], at (0, j) == Just ch]
        isEnd (i, j) = case mark of
            Cross -> j == w - 1
            Nought -> i == h - 1
        reachable = flood mempty (Set.fromList starts)
        flood visited frontier
            | null frontier = visited
            | otherwise = flood visited' frontier'
          where
            visited' = visited <> frontier
            frontier' =
                Set.fromList
                    [ n
                    | p <- toList frontier
                    , n <- neighbors p
                    , at n == Just ch
                    , n `notElem` visited'
                    ]

symbol :: Mark -> Char
symbol Cross = 'X'
symbol Nought = 'O'

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) =
    [ (i - 1, j)
    , (i - 1, j + 1)
    , (i, j - 1)
    , (i, j + 1)
    , (i + 1, j - 1)
    , (i + 1, j)
    ]
