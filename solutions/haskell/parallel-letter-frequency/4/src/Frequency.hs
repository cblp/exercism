{-# LANGUAGE ImportQualifiedPost #-}

module Frequency (frequency) where

import Control.Parallel.Strategies
import Data.Char (isAlpha, toLower)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
    Map.unionsWith (+) $
        case nWorkers of
            1 -> map frequencyChunk texts
            _ -> parMap rdeepseq frequencyChunk texts
  where
    frequencyChunk text =
        Map.fromListWith (+) [(toLower c, 1) | c <- Text.unpack text, isAlpha c]
