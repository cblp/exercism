{-# LANGUAGE LambdaCase #-}

module Affine (decode, encode) where

import Data.Char (chr, isDigit, isLetter, ord, toLower)

isCoprimeWith26 :: Int -> Bool
isCoprimeWith26 = (`elem` [1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25])

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
    | isCoprimeWith26 a = Just $ go plainText (0 :: Int)
    | otherwise = Nothing
  where
    go [] _ = []
    go (x : xs) n
        | isLetter x =
            let i = ord (toLower x) - ord 'a'
                e = (a * i + b) `mod` 26
            in  space n ++ chr (ord 'a' + e) : go xs (n + 1)
        | isDigit x = space n ++ x : go xs (n + 1)
        | otherwise = go xs n

    space n
        | n /= 0 && n `mod` 5 == 0 = " "
        | otherwise = ""

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
    | isCoprimeWith26 a = Just $ go cipherText
    | otherwise = Nothing
  where
    a' = inv a
    go [] = []
    go (x : xs)
        | isLetter x =
            let y = ord (toLower x) - ord 'a'
                d = (a' * (y - b)) `mod` 26
            in  chr (ord 'a' + d) : go xs
        | isDigit x = x : go xs
        | otherwise = go xs

inv :: Int -> Int
inv = \case
    1 -> 1
    3 -> 9
    5 -> 21
    7 -> 15
    9 -> 3
    11 -> 19
    15 -> 7
    17 -> 23
    19 -> 11
    21 -> 5
    23 -> 17
    25 -> 25
    _ -> error "impossible"
