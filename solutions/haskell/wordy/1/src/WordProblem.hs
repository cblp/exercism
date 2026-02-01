module WordProblem (answer) where

import Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer = eval . drop 2 . words . init

eval :: [String] -> Maybe Integer
eval [s] = readMaybe s
eval xs
    | (x, ["plus", y]) <- splitEnd 2 = go (+) x y
    | (x, ["minus", y]) <- splitEnd 2 = go (-) x y
    | (x, ["multiplied", "by", y]) <- splitEnd 3 = go (*) x y
    | (x, ["divided", "by", y]) <- splitEnd 3 = go div x y
    | otherwise = Nothing
  where
    splitEnd n = splitAt (length xs - n) xs
    go op x y = op <$> eval x <*> readMaybe y
