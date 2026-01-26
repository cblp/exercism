{-# LANGUAGE BlockArguments #-}

module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n < 1 = Nothing
    | n == 1 = Just Deficient
    | otherwise =
        Just
            case compare s n of
                LT -> Deficient
                EQ -> Perfect
                GT -> Abundant
  where
    s =
        sum
            [ if x * x == n then x else x + (n `div` x)
            | x <- takeWhile (\x -> x * x <= n) [2 ..]
            , n `mod` x == 0
            ]
            + 1
