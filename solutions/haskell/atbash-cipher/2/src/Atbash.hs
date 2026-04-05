{-# LANGUAGE LambdaCase #-}

module Atbash (decode, encode) where

import Data.Char (chr, isAlpha, isAlphaNum, ord, toLower)

decode :: String -> String
decode = map mirrorAny . filter isAlphaNum

encode :: String -> String
encode = unwords . chunksOf 5 . map (mirrorAny . toLower) . filter isAlphaNum

mirrorAny :: Char -> Char
mirrorAny c = if isAlpha c then mirrorLetter c else c

mirrorLetter :: Char -> Char
mirrorLetter = chr . (az -) . ord

az :: Int
az = ord 'a' + ord 'z'

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = \case
    [] -> []
    xs -> let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest
