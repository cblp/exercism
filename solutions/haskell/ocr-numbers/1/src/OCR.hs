{-# LANGUAGE LambdaCase #-}

module OCR (convert) where

import Control.Arrow ((>>>))
import Data.List (intercalate, transpose)

convert :: String -> String
convert =
    lines
        >>> chunksOf 4
        >>> map (transpose >>> chunksOf 3 >>> map recognizeDigit)
        >>> intercalate ","
  where
    recognizeDigit :: [String] -> Char
    recognizeDigit =
        map (take 3)
            >>> concat
            >>> \case
                _ : a : b : c : d : _ : _ : g : _ -> [a, b, c, d, g]
                _ -> []
            >>> \case
                "||_ |" -> '0'
                "    |" -> '1'
                " |__|" -> '2'
                "  __|" -> '3'
                "|  _|" -> '4'
                "| __ " -> '5'
                "||__ " -> '6'
                "  _ |" -> '7'
                "||__|" -> '8'
                "| __|" -> '9'
                _ -> '?'

    chunksOf i xs =
        let (t, d) = splitAt i xs in case d of [] -> [t]; _ -> t : chunksOf i d
