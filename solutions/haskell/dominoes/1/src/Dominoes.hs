{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Dominoes (chain) where

import Prelude hiding (head, last)

import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (delete)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain = \case
    [] -> Just []
    (head, startB) : dominoes -> go [(startB, head)] startB dominoes
      where
        go :: [(Int, Int)] -> Int -> [(Int, Int)] -> Maybe [(Int, Int)]
        go chained last = \case
            [] -> guard (head == last) $> chained
            free ->
                asum
                    [ if
                        | a == last -> go ((b, a) : chained) b $ delete d free
                        | b == last -> go ((a, b) : chained) a $ delete d free
                        | otherwise -> Nothing
                    | d@(a, b) <- free
                    ]
