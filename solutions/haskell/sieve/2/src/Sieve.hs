module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, divMod, mod, quot, quotRem, rem, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n = iterSieve [2 .. n]
  where
    iterSieve [] = []
    iterSieve (p : xs) = p : iterSieve (xs \\ [p, p * 2 .. n])

(\\) :: (Ord a) => [a] -> [a] -> [a]
[] \\ _ = []
xs \\ [] = xs
(x : xs) \\ (y : ys) =
    case compare x y of
        LT -> x : (xs \\ (y : ys))
        EQ -> xs \\ ys
        GT -> (x : xs) \\ ys
