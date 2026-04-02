{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Spiral (spiral) where

import Control.Monad (foldM_)
import Control.Monad.ST (runST)
import Data.Vector.Unboxed ((!))
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector

data Direction = R | D | L | U deriving (Enum, Eq, Show)

spiral :: Int -> [[Int]]
spiral 0 = []
spiral size =
    toLists $ runST do
        matrix <- MVector.replicate (size * size) 0
        let inBounds (r, c) = r >= 0 && r < size && c >= 0 && c < size
            turn d = toEnum $ (fromEnum d + 1) `mod` 4
            step d pos = do
                let fwd = next pos d
                    d' = turn d
                canGo <-
                    if inBounds fwd then
                        (== 0) <$> MVector.read matrix (idx fwd)
                    else
                        pure False
                pure if canGo then (d, fwd) else (d', next pos d')
        foldM_
            (\(d, pos) n -> MVector.write matrix (idx pos) n >> step d pos)
            (R, (0, 0))
            [1 .. size * size :: Int]
        Vector.freeze matrix
  where
    idx (r, c) = r * size + c
    toLists v =
        [[v ! idx (r, c) | c <- [0 .. size - 1]] | r <- [0 .. size - 1]]

next :: (Int, Int) -> Direction -> (Int, Int)
next (i, j) = \case
    R -> (i, j + 1)
    D -> (i + 1, j)
    L -> (i, j - 1)
    U -> (i - 1, j)
