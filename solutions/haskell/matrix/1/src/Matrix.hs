{-# LANGUAGE ImportQualifiedPost #-}

module Matrix (
    Matrix,
    cols,
    column,
    flatten,
    fromList,
    fromString,
    reshape,
    row,
    rows,
    shape,
    transpose,
) where

import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector

newtype Matrix a = M (Vector (Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (M rs) = case length rs of 0 -> 0; _ -> length $ rs ! 0

column :: Int -> Matrix a -> Vector a
column x (M rs) = Vector.map (! (x - 1)) rs

flatten :: Matrix a -> Vector a
flatten (M rs) = Vector.concat $ Vector.toList rs

fromList :: [[a]] -> Matrix a
fromList = M . Vector.fromList . map Vector.fromList

fromString :: (Read a) => String -> Matrix a
fromString = fromList . map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (rowN', colN') m =
    M $
        Vector.fromList
            [ Vector.fromList [flat ! (i * colN' + j) | j <- [0 .. colN' - 1]]
            | i <- [0 .. rowN' - 1]
            ]
  where
    flat = flatten m

row :: Int -> Matrix a -> Vector a
row x (M rs) = rs ! (x - 1)

rows :: Matrix a -> Int
rows (M rs) = length rs

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose m@(M rs) =
    M $
        Vector.fromList
            [ Vector.fromList [rs ! i ! j | i <- [0 .. rows m - 1]]
            | j <- [0 .. cols m - 1]
            ]
