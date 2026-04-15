{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Connect (Mark (..), winner) where

import Control.Lens (ix, preuse, (.=))
import Control.Monad (when)
import Control.Monad.State.Strict (evalState, gets)
import Control.Monad.Writer.Strict (MonadWriter, execWriterT, tell)
import Data.Foldable (fold, for_, toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
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

yield :: (MonadWriter (Seq a) m) => a -> m ()
yield = tell . Seq.singleton

enumerateClusters :: Board -> [Cluster]
enumerateClusters board =
    toList . (`evalState` board) . execWriterT $ do
        foldFor (zip [0 ..] board) \(i, row) -> do
            foldFor (zip [0 ..] row) \(j, _) -> do
                cell <- gets $ (!! j) . (!! i)
                case cell of
                    'X' -> do
                        c <- execWriterT $ takeCluster Cross (i, j)
                        yield (Cross, Set.fromList $ toList c)
                    'O' -> do
                        c <- execWriterT $ takeCluster Nought (i, j)
                        yield (Nought, Set.fromList $ toList c)
                    _ -> pure ()
  where
    takeCluster mark p@(i, j) = do
        mcell <- preuse $ ix i . ix j
        when (mcell == Just (symbol mark)) do
            ix i . ix j .= '.'
            yield p
            for_ (neighbors p) $ takeCluster mark

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
        Cross
            | any (\(_, j) -> j == 0) cluster
            , any (\(_, j) -> j == width - 1) cluster ->
                CrossWon
        Nought
            | any (\(i, _) -> i == 0) cluster
            , any (\(i, _) -> i == height - 1) cluster ->
                NoughtWon
        _ ->
            Nobody

foldFor :: (Applicative m, Monoid b, Traversable f) => f a -> (a -> m b) -> m b
foldFor xs f = fold <$> for xs f
