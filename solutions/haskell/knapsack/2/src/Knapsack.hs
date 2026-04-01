module Knapsack (maximumValue) where

import Data.Map.Strict (Map, assocs, fromListWith, singleton, unionWith)
import Data.Semigroup

newtype MonoidMap k a = MonoidMap (Map k a)

instance (Ord k, Semigroup a) => Semigroup (MonoidMap k a) where
    MonoidMap a <> MonoidMap b = MonoidMap $ unionWith (<>) a b

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue maxWeight =
    getMax
        . snd
        . foldl
            ( \s@(best, _) (itemWeight, itemValue) ->
                let newItems =
                        [ (w', v')
                        | (w, v) <- assocs' best
                        , let w' = w + itemWeight
                        , w' <= maxWeight
                        , let v' = v + Max itemValue
                        ]
                 in s <> (fromList' newItems, foldMap snd newItems)
            )
            (MonoidMap $ singleton 0 $ Max 0, Max 0)
  where
    assocs' (MonoidMap m) = assocs m
    fromList' = MonoidMap . fromListWith (<>)
