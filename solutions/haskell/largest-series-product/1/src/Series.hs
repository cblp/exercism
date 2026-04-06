module Series (Error (..), largestProduct) where

import Data.Char (digitToInt)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Eq, Show)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits_ = do
    digits <- traverse digitToInteger digits_
    let digitGroups = slidingWindow size digits
    case digitGroups of
        [] -> Left InvalidSpan
        _ -> Right . maximum . fmap product $ digitGroups

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n = filter ((n ==) . length) . map (take n) . tails

digitToInteger :: Char -> Either Error Integer
digitToInteger c
    | '0' <= c, c <= '9' = Right $ toInteger $ digitToInt c
    | otherwise = Left $ InvalidDigit c
