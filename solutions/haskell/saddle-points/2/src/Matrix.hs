{-# LANGUAGE BlockArguments #-}

module Matrix (saddlePoints) where

import Data.Array

saddlePoints :: (Ord e) => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix =
    [(i, j) | i <- i_range, j <- find_max_indexes i, is_min_in_column i j]
  where
    ((i_min, j_min), (i_max, j_max)) = bounds matrix
    i_range = [i_min .. i_max]
    row i = ixmap (j_min, j_max) (i,) matrix
    column j = ixmap (i_min, i_max) (,j) matrix

    find_max_indexes i = [j | (j, v) <- assocs ri, v == maxv]
      where
        ri = row i
        maxv = maximum ri

    is_min_in_column i j = all (>= v) $ column j where v = matrix ! (i, j)
