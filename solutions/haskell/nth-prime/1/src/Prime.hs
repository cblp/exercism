{-# LANGUAGE LambdaCase #-}

module Prime (nth) where

nth :: Int -> Maybe Integer
nth n | n < 1 = Nothing
nth 1 = Just 2
nth 2 = Just 3
nth n = Just $ toInteger $ go (3 :+ 2 :- Nil) (n - 2)

data StrictInts = Nil | !Int :- StrictInts
data NonEmptyStrictInts = !Int :+ StrictInts
infixr 5 :-, :+

toList :: StrictInts -> [Int]
toList = \case
    Nil -> []
    x :- xs -> x : toList xs

go :: NonEmptyStrictInts -> Int -> Int
go (lastKP :+ otherKPs) = \case
    0 -> lastKP
    n -> go (nextPrime :+ lastKP :- otherKPs) (n - 1)
  where
    nextPrime =
        head
            [ x
            | x <- [lastKP + 2, lastKP + 4 ..]
            , and [x `mod` p /= 0 | p <- lastKP : toList otherKPs]
            ]

-- error
-- \$ unwords ["n =", show n, ", KPs =", show KPs]

-- PRIMES = [2, 3]

-- def prime(n: int) -> int:
--     if n < 1:
--         raise ValueError("there is no zeroth prime")
--     while n > len(PRIMES):
--         for x in count(PRIMES[-1] + 2, 2):
--             if all(x % p for p in PRIMES):
--                 PRIMES.append(x)
--                 break
--     return PRIMES[n - 1]
