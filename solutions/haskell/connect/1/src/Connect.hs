{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Connect (Mark (..), winner) where

import Control.Lens (ix, preuse, (.=))
import Control.Monad.State.Strict (State, evalState, gets)
import Data.Foldable (fold)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)

data Mark = Cross | Nought deriving (Eq, Show)

data Winner = Nobody | CrossWon | NoughtWon | Both deriving (Eq, Show)

instance Semigroup Winner where
    Nobody <> x = x
    x <> Nobody = x
    x <> y | x == y = x
    _ <> _ = Both

instance Monoid Winner where mempty = Nobody

symbol :: Mark -> Char
symbol = \case Cross -> 'X'; Nought -> 'O'

type Board = [[Char]]

type Cluster = (Mark, Set (Int, Int))

winner :: Board -> Maybe Mark
winner board_ =
    case foldMap (winners (height, width)) $ enumerateClusters board of
        CrossWon -> Just Cross
        NoughtWon -> Just Nought
        _ -> Nothing
  where
    board = map (filter (/= ' ')) board_
    height = length board
    width = case board of [] -> 0; firstRow : _rows -> length firstRow

enumerateClusters :: Board -> [Cluster]
enumerateClusters board =
    (`evalState` board) do
        foldFor (zip [0 ..] board) \(i, row) -> do
            foldFor (zip [0 ..] row) \(j, _) -> do
                cell <- gets $ (!! j) . (!! i)
                case cell of
                    'X' -> do
                        c <- takeCluster Cross (i, j)
                        pure [(Cross, Set.fromList c)]
                    'O' -> do
                        c <- takeCluster Nought (i, j)
                        pure [(Nought, Set.fromList c)]
                    _ -> pure []
  where
    takeCluster :: Mark -> (Int, Int) -> State Board [(Int, Int)]
    takeCluster mark p@(i, j) = do
        mcell <- preuse $ ix i . ix j
        if mcell == Just (symbol mark) then do
            ix i . ix j .= '.'
            fmap ((p :) . fold) $ for (neighbors p) $ takeCluster mark
        else
            pure []

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) =
    fold
        [ [(i - 1, j), (i - 1, j + 1)]
        , --           \    /
          [(i, j - 1 {- i, j -}), (i, j + 1)]
        , --           /    \
          [(i + 1, j - 1), (i + 1, j)]
        ]

winners :: (Int, Int) -> Cluster -> Winner
winners (height, width) (mark, cluster) =
    case mark of
        Cross ->
            if any (\(_, j) -> j == 0) cluster
                && any (\(_, j) -> j == width - 1) cluster
            then
                CrossWon
            else
                Nobody
        Nought ->
            if any (\(i, _) -> i == 0) cluster
                && any (\(i, _) -> i == height - 1) cluster
            then
                NoughtWon
            else
                Nobody

foldFor :: (Applicative m, Monoid b, Traversable f) => f a -> (a -> m b) -> m b
foldFor xs f = fold <$> for xs f

{-
 0 1 2 3
 \-\-\-\-
0 \. O . .
 1 \O X X X
  2 \O X O .
   3 \X X O X
    4 \. O X .
-}
