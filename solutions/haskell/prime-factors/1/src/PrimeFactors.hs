module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors = go [] [2 ..]
  where
    go r _ 1 = r
    go r candidates@(x : rest) n =
        case n `divMod` x of
            (n', 0) -> go (r ++ [x]) candidates n'
            _ -> go r rest n
    go _ [] _ = error "impossible"
