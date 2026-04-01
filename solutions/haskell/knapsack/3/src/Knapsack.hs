module Knapsack (maximumValue) where

import Data.Map.Strict (assocs, fromListWith, singleton, unionWith)

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue maxWeight =
    snd
        . foldl
            ( \(best, maxValue) item ->
                let new = best +^ item in (best <>= new, maxValue ^= new)
            )
            (singleton 0 0, 0)
  where
    best <>= new = unionWith max best $ fromListWith max new
    maxValue ^= new = maximum $ maxValue : map snd new
    best +^ (itemWeight, itemValue) =
        [ (w', v')
        | (w, v) <- assocs best
        , let w' = w + itemWeight
        , w' <= maxWeight
        , let v' = v + itemValue
        ]
