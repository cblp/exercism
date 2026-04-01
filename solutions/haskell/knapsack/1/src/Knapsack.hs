{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Knapsack (maximumValue) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.HashMap.Strict qualified as HashMap
import Data.STRef

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue _ [] = 0
maximumValue maxWeight items =
    runST do
        best <- newSTRef (HashMap.singleton 0 0)
        maxValue <- newSTRef 0
        for_ items \(itemWeight, itemValue) -> do
            bestBeforeThisItem <- readSTRef best
            for_
                (HashMap.toList bestBeforeThisItem)
                \(bestWeight, bestValue) -> do
                    let newWeight = bestWeight + itemWeight
                    when (newWeight <= maxWeight) do
                        let newValue = bestValue + itemValue
                        modifySTRef' best $
                            HashMap.insertWith max newWeight newValue
                        modifySTRef' maxValue $ max newValue
        readSTRef maxValue
