{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spiral (spiral) where

import Control.Monad (foldM_)
import Data.Array.ST.Safe (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed ((!))
import Data.Ix (inRange)

data Direction = R | D | L | U deriving (Enum, Eq, Show)

spiral :: Int -> [[Int]]
spiral 0 = []
spiral size =
    toLists $ runSTUArray do
        matrix <- newArray bounds 0
        let turn d = toEnum $ (fromEnum d + 1) `mod` 4
            step d pos = do
                let fwd = next pos d
                    d' = turn d
                canGo <-
                    if inRange bounds fwd then
                        (== 0) <$> readArray matrix fwd
                    else
                        pure False
                pure $ if canGo then (d, fwd) else (d', next pos d')
        foldM_
            ( \(d, pos) n -> do
                writeArray matrix pos n
                step d pos
            )
            (R, (0, 0))
            [1 .. size * size :: Int]
        pure matrix
  where
    bounds = ((0, 0), (size - 1, size - 1))
    toLists arr = [[arr ! (r, c) | c <- [0 .. size - 1]] | r <- [0 .. size - 1]]

next :: (Int, Int) -> Direction -> (Int, Int)
next (i, j) = \case
    R -> (i, j + 1)
    D -> (i + 1, j)
    L -> (i, j - 1)
    U -> (i - 1, j)
