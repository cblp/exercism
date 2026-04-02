{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spiral (spiral) where

import Data.Array
import Data.Array.ST

data Direction = R | D | L | U deriving (Enum, Eq, Show)

spiral :: Int -> [[Int]]
spiral 0 = []
spiral size =
    toLists $ runSTArray do
        matrix <- newArray ((0, 0), (size - 1, size - 1)) 0
        let go d (i, j) n
                | n > size * size = pure ()
                | otherwise = do
                    writeArray matrix (i, j) n
                    let (ni, nj) = next (i, j) d
                    let d' = toEnum $ (fromEnum d + 1) `mod` 4
                    if ni < 0 || ni >= size || nj < 0 || nj >= size then
                        go d' (next (i, j) d') (n + 1)
                    else do
                        v <- readArray matrix (ni, nj)
                        if v /= 0 then
                            go d' (next (i, j) d') (n + 1)
                        else
                            go d (ni, nj) (n + 1)
        go R (0, 0) (1 :: Int)
        pure matrix

next :: (Int, Int) -> Direction -> (Int, Int)
next (i, j) = \case
    R -> (i, j + 1)
    D -> (i + 1, j)
    L -> (i, j - 1)
    U -> (i - 1, j)

toLists :: Array (Int, Int) a -> [[a]]
toLists arr = [[arr ! (r, c) | c <- [cMin .. cMax]] | r <- [rMin .. rMax]]
  where
    ((rMin, cMin), (rMax, cMax)) = bounds arr
