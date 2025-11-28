{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Frequency (frequency) where

import Control.Parallel.Strategies

-- import Control.Exception (evaluate)
-- import UnliftIO.Async (pooledMapConcurrentlyN)
-- import System.IO.Unsafe (unsafePerformIO)

import Data.Char (isAlpha, toLower)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

frequencyChunk :: Text -> Map Char Int
frequencyChunk =
    Map.fromListWith (+) . map ((,1) . toLower) . filter isAlpha . Text.unpack

frequency :: Int -> [Text] -> Map Char Int
frequency 1 texts = Map.unionsWith (+) (map frequencyChunk texts)
frequency nWorkers texts =
    Map.unionsWith (+) $
        withStrategy (parListChunk (length texts `mod` nWorkers) rseq) $
            map frequencyChunk texts

-- frequency :: Int -> [Text] -> Map Char Int
-- frequency nWorkers texts =
--     unsafePerformIO $
--         Map.unionsWith (+)
--             <$> pooledMapConcurrentlyN
--                 nWorkers
--                 (evaluate . frequencyChunk)
--                 texts
