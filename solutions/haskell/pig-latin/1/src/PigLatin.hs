{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module PigLatin (translate) where

translate :: String -> String
translate = unwords . map pig . words
  where
    pig word = case word of
        a : _ | a `elem` "aeiou" -> word ++ "ay"
        a : b : _ | [a, b] `elem` ["xr", "yt"] -> word ++ "ay"
        (breakAfterConsonants -> (b, c)) -> c ++ b ++ "ay"

    breakAfterConsonants = \case
        'y' : xs -> let (ys, zs) = breakAfterConsonants' xs in ('y' : ys, zs)
        xs -> breakAfterConsonants' xs

    breakAfterConsonants' = \case
        [] -> ([], [])
        'q' : 'u' : xs -> ("qu", xs)
        xs@(x : _) | x `elem` "aeiouy" -> ([], xs)
        x : xs -> let (ys, zs) = breakAfterConsonants' xs in (x : ys, zs)
