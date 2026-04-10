{-# LANGUAGE LambdaCase #-}

module BookStore (total, Book (..)) where

import Control.Monad.State.Strict (MonadState, State, evalState, gets, modify)
import Data.List (dropWhileEnd, nub, sortOn)
import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))

data Book = First | Second | Third | Fourth | Fifth
    deriving (Eq, Ord)

type BasketNormalized = [Int]

total :: [Book] -> Int
total = (`evalState` Map.empty) . total_memoized . count

total_memoized :: BasketNormalized -> State (Map BasketNormalized Int) Int
total_memoized = memoized total_impl

memoized :: (MonadState (Map a b) m, Ord a) => (a -> m b) -> a -> m b
memoized f x =
    gets (!? x) >>= \case
        Just value -> pure value
        Nothing -> do
            value <- f x
            modify $ Map.insert x value
            pure value

normalize :: [Int] -> BasketNormalized
normalize = chop . sortOn Down

chop :: [Int] -> BasketNormalized
chop = dropWhileEnd (== 0)

count :: (Ord a) => [a] -> BasketNormalized
count = sortOn Down . Map.elems . Map.fromListWith (+) . map (,1)

cost1 :: Int
cost1 = 8_00

cost2 :: Int
cost2 = 15_20

cost3 :: Int
cost3 = 21_60

cost4 :: Int
cost4 = 25_60

cost5 :: Int
cost5 = 30_00

total_impl :: BasketNormalized -> State (Map BasketNormalized Int) Int
total_impl = \case
    [] -> pure 0
    [b] -> pure $ cost1 * b
    basket ->
        minimum
            <$> sequence
                [ (costN +) <$> total_memoized withoutN
                | (n, costN) <- [(5, cost5), (4, cost4), (3, cost3), (2, cost2)]
                , withoutN <- takeSome n basket
                ]

takeSome :: Int -> BasketNormalized -> [BasketNormalized]
takeSome n basket =
    case compare n (length basket) of
        GT -> []
        EQ -> [chop $ map pred basket]
        LT -> nub $ map normalize $ takeSomeVariants n basket

takeSomeVariants :: Int -> BasketNormalized -> [[Int]]
takeSomeVariants 2 [b1, b2, b3] =
    [[b1 - 1, b2 - 1, b3], [b1 - 1, b2, b3 - 1], [b1, b2 - 1, b3 - 1]]
takeSomeVariants 2 [b1, b2, b3, b4] =
    [ [b1 - 1, b2 - 1, b3, b4]
    , [b1 - 1, b2, b3 - 1, b4]
    , [b1 - 1, b2, b3, b4 - 1]
    , [b1, b2 - 1, b3 - 1, b4]
    , [b1, b2 - 1, b3, b4 - 1]
    , [b1, b2, b3 - 1, b4 - 1]
    ]
takeSomeVariants 3 [b1, b2, b3, b4] =
    [ [b1 - 1, b2 - 1, b3 - 1, b4]
    , [b1 - 1, b2 - 1, b3, b4 - 1]
    , [b1 - 1, b2, b3 - 1, b4 - 1]
    , [b1, b2 - 1, b3 - 1, b4 - 1]
    ]
takeSomeVariants 2 [b1, b2, b3, b4, b5] =
    [ [b1 - 1, b2 - 1, b3, b4, b5]
    , [b1 - 1, b2, b3 - 1, b4, b5]
    , [b1 - 1, b2, b3, b4 - 1, b5]
    , [b1 - 1, b2, b3, b4, b5 - 1]
    , [b1, b2 - 1, b3 - 1, b4, b5]
    , [b1, b2 - 1, b3, b4 - 1, b5]
    , [b1, b2 - 1, b3, b4, b5 - 1]
    , [b1, b2, b3 - 1, b4 - 1, b5]
    , [b1, b2, b3 - 1, b4, b5 - 1]
    , [b1, b2, b3, b4 - 1, b5 - 1]
    ]
takeSomeVariants 3 [b1, b2, b3, b4, b5] =
    [ [b1 - 1, b2 - 1, b3 - 1, b4, b5]
    , [b1 - 1, b2 - 1, b3, b4 - 1, b5]
    , [b1 - 1, b2 - 1, b3, b4, b5 - 1]
    , [b1 - 1, b2, b3 - 1, b4 - 1, b5]
    , [b1 - 1, b2, b3 - 1, b4, b5 - 1]
    , [b1 - 1, b2, b3, b4 - 1, b5 - 1]
    , [b1, b2 - 1, b3 - 1, b4 - 1, b5]
    , [b1, b2 - 1, b3 - 1, b4, b5 - 1]
    , [b1, b2 - 1, b3, b4 - 1, b5 - 1]
    , [b1, b2, b3 - 1, b4 - 1, b5 - 1]
    ]
takeSomeVariants 4 [b1, b2, b3, b4, b5] =
    [ [b1 - 1, b2 - 1, b3 - 1, b4 - 1, b5]
    , [b1 - 1, b2 - 1, b3 - 1, b4, b5 - 1]
    , [b1 - 1, b2 - 1, b3, b4 - 1, b5 - 1]
    , [b1 - 1, b2, b3 - 1, b4 - 1, b5 - 1]
    , [b1, b2 - 1, b3 - 1, b4 - 1, b5 - 1]
    ]
takeSomeVariants n b = error $ show (n, b)
