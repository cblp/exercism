{-# LANGUAGE ParallelListComp #-}

module FoodChain (song) where

import Data.List (intercalate, tails)
import Text.Printf (printf)

song :: String
song =
    intercalate
        "\n"
        [ unlines $ first object : [second | not $ null second] ++ other
        | (object, second) <- objects
        | other <- tail (reverse $ tails others) ++ [[]]
        ]
  where
    objects =
        [ ("fly", "")
        , ("spider", "It wriggled and jiggled and tickled inside her.")
        , ("bird", "How absurd to swallow a bird!")
        , ("cat", "Imagine that, to swallow a cat!")
        , ("dog", "What a hog, to swallow a dog!")
        , ("goat", "Just opened her throat and swallowed a goat!")
        , ("cow", "I don't know how she swallowed a cow!")
        , ("horse", "She's dead, of course!")
        ]
    first = printf "I know an old lady who swallowed a %s."
    others =
        [ "She swallowed the cow to catch the goat."
        , "She swallowed the goat to catch the dog."
        , "She swallowed the dog to catch the cat."
        , "She swallowed the cat to catch the bird."
        , "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
        , "She swallowed the spider to catch the fly."
        , "I don't know why she swallowed the fly. Perhaps she'll die."
        ]
