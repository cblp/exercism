module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n < 1     = Nothing
    | otherwise = Just $ collatz' n

collatz' :: Integer -> Integer
collatz' n
    | n == 1    = 0
    | even n    = 1 + collatz' (n `div` 2)
    | otherwise = 1 + collatz' (3 * n + 1)
