{-# LANGUAGE ParallelListComp #-}

module Proverb (recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite items =
    intercalate
        "\n"
        (   [ "For want of a " <> a <> " the " <> b <> " was lost."
            | a <- items
            | b <- tail items
            ]
        ++  ["And all for the want of a " <> item <> "." | item <- take 1 items]
        )
