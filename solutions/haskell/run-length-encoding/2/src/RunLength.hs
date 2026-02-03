{-# LANGUAGE LambdaCase #-}

module RunLength (decode, encode) where

import Data.Char (digitToInt, isDigit)

decode :: String -> String
decode = go 0
  where
    go n = \case
        "" -> ""
        t : ext
            | isDigit t -> go (n * 10 + digitToInt t) ext
            | otherwise -> replicate (max 1 n) t ++ go 0 ext

encode :: String -> String
encode = \case
    "" -> ""
    t : ext -> go t 1 ext
  where
    go c n = \case
        [] -> out c n ""
        x : xs
            | c == x -> go c (n + 1) xs
            | otherwise -> out c n $ go x 1 xs

    out c = \case
        1 -> (c :)
        n -> (show (n :: Word) ++) . (c :)
