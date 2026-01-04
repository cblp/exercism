{-# LANGUAGE LambdaCase #-}

module Prime (nth) where

nth :: Int -> Maybe Integer
nth = \case
    n | n < 1 -> Nothing
    1 -> Just 2
    2 -> Just 3
    n -> Just $ toInteger $ go (3, 2 :. Nil) (n - 2)

data StrictInts = Nil | !Int :. !StrictInts
infixr 5 :.

toList :: StrictInts -> [Int]
toList = \case
    Nil -> []
    x :. xs -> x : toList xs

go :: (Int, StrictInts) -> Int -> Int
go (lastKP, otherKPs) = \case
    0 -> lastKP
    n -> go (nextPrime, lastKP :. otherKPs) (n - 1)
  where
    nextPrime =
        head
            [ x
            | x <- [lastKP + 2, lastKP + 4 ..]
            , and [x `mod` p /= 0 | p <- lastKP : toList otherKPs]
            ]
