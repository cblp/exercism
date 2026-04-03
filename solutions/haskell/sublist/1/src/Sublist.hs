module Sublist (sublist) where

import Data.List (isInfixOf)

sublist :: (Eq a) => [a] -> [a] -> Maybe Ordering
sublist xs ys =
    case compare (length xs) (length ys) of
        EQ | xs == ys -> Just EQ
        LT | xs `isInfixOf` ys -> Just LT
        GT | ys `isInfixOf` xs -> Just GT
        _ -> Nothing
