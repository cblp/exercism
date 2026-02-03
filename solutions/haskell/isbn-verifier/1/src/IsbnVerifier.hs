{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Traversable (for)

isbn :: String -> Bool
isbn input = length digits == 10 && checkSum
  where
    digits = filter (/= '-') input
    checkSum =
        ( do
            xs <-
                for (zip [10, 9 ..] digits) \case
                    (1, 'X') -> Just 10
                    (factor, c) | isDigit c -> Just $ factor * digitToInt c
                    _ -> Nothing
            pure $ sum xs `mod` 11 == 0
        )
            & fromMaybe False
