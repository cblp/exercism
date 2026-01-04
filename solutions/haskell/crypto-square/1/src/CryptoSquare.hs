{-# LANGUAGE LambdaCase #-}

module CryptoSquare (encode) where

import Data.Char (isDigit, isLetter, toLower)
import Data.List (transpose)

encode :: String -> String
encode plainText
    | null message = ""
    | otherwise = unwords cipherSquare
  where
    message = [toLower c | c <- plainText, isLetter c || isDigit c]
    messageLength = length message
    cols = head [c | r <- [0 ..], c <- [r, r + 1], c * r >= messageLength]
    plainSquare = chunksOf cols message
    lastLine = last plainSquare
    plainSquarePadded =
        init plainSquare ++ [lastLine ++ replicate (cols - length lastLine) ' ']
    cipherSquare = transpose plainSquarePadded

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = \case
    [] -> []
    xs -> t : chunksOf n d where (t, d) = splitAt n xs
