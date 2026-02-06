{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}

module Diamond (diamond) where

import Data.Char (isLetter, ord)
import Data.Text (Text, pack)
import Text.Printf (printf)

diamond :: Char -> Maybe [Text]
diamond c = [mirror $ map (pack . mirror . row) [0 .. n] | isLetter c]
  where
    n = ord c - 65
    row = \case
        0 -> printf "%*c" (n + 1) 'A'
        i -> printf "%*c%*s" (n - i + 1) (65 + i) i ""
    mirror xs = init xs ++ reverse xs
