{-# LANGUAGE LambdaCase #-}

module Accumulate (accumulate) where

accumulate :: (Show a) => (a -> b) -> [a] -> [b]
accumulate f = \case
    [] -> []
    x : xs -> f x : accumulate f xs
