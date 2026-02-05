module Raindrops (convert) where

convert :: Int -> String
convert n = ("Pling" // 3 ++ "Plang" // 5 ++ "Plong" // 7) ?: show n
  where
    a // b = if n `mod` b == 0 then a else ""
    a ?: b = case a of [] -> b; _ -> a
