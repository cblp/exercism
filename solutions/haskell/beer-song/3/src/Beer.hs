{-# LANGUAGE LambdaCase #-}

module Beer (song) where

import Data.Char (toUpper)
import Text.Printf (printf)

song :: String
song = drop 1 $ concatMap verse [99, 98 .. 0 :: Int]
  where
    verse n = printf "\n%s, %s.\n%s, %s.\n" part1 part2 part3 part4
      where
        part1 = titleCase part2 <> " on the wall"

        part2 = bottlesOfBeer n

        part3 = case n of
            0 -> "Go to the store and buy some more"
            1 -> "Take it down and pass it around"
            _ -> "Take one down and pass it around"

        part4 = bottlesOfBeer ((n - 1) `mod` 100) <> " on the wall"

    bottlesOfBeer n = printf "%s bottle%s of beer" (showBottles n) (suffix n)

    suffix = \case 1 -> ""; _ -> "s"

    showBottles = \case 0 -> "no more"; n -> show n

    titleCase = \case x : xs -> toUpper x : xs; "" -> ""
