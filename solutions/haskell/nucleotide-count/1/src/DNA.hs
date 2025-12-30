{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts =
    fmap (Map.fromListWith (+)) . traverse (fmap (,1) . nucleotideRead)

nucleotideRead :: Char -> Either String Nucleotide
nucleotideRead = \case
    'A' -> Right A
    'C' -> Right C
    'G' -> Right G
    'T' -> Right T
    x   -> Left $ "Invalid nucleotide: " ++ [x]
