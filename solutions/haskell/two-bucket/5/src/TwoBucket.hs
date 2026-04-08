{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module TwoBucket (measure) where

import Control.Lens (ASetter', use, (%=), (.=), (|>))
import Control.Lens.TH (makeClassy_)
import Control.Monad (when)
import Control.Monad.Except (MonadError, runExcept, throwError)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State (MonadState, evalStateT)
import Data.Function ((&))
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

type TaskState = (Int, (Int, Int))

data AlgoState = AlgoState {visited :: Set (Int, Int), queue :: Seq TaskState}
    deriving (Show)
makeClassy_ ''AlgoState

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (capacity1, capacity2) target =
    evalStateT measure' initialAlgoState
        & runExcept
        & either Just (\() -> Nothing)
  where
    forbiddenBuckets = (0, capacity2)

    measure' = do
        _queue |>= (1, (capacity1, 0)) -- start with filling the first bucket
        whileJust pop \gameState@(moves, buckets@(bucket1, bucket2)) -> do
            when (target `elem` [bucket1, bucket2]) $ stopWith gameState
            _visited %= Set.insert buckets
            runReaderT (addNextMoves buckets) (moves + 1)

    add buckets = do
        v <- use _visited
        when (buckets `notElem` v && buckets /= forbiddenBuckets) do
            step <- ask
            _queue |>= (step, buckets)

    addNextMoves (bucket1, bucket2) = do
        when (bucket1 < capacity1) do
            when (bucket2 > 0) do
                let d = min (capacity1 - bucket1) bucket2
                add (bucket1 + d, bucket2 - d) -- pour snd -> fst
            add (capacity1, bucket2) -- fill fst
        when (bucket2 < capacity2) do
            when (bucket1 > 0) do
                let d = min (capacity2 - bucket2) bucket1
                add (bucket1 - d, bucket2 + d) -- pour fst -> snd
            add (bucket1, capacity2) -- fill snd
        when (bucket1 > 0) $ add (0, bucket2) -- empty fst
        when (bucket2 > 0) $ add (bucket1, 0) -- empty snd

pop :: (MonadState AlgoState m) => m (Maybe TaskState)
pop = do
    q <- use _queue
    case q of
        Seq.Empty -> pure Nothing
        s :<| q' -> do
            _queue .= q'
            pure $ Just s

whileJust :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whileJust get action = loop
  where
    loop =
        get >>= \case
            Nothing -> pure ()
            Just t -> action t *> loop

initialAlgoState :: AlgoState
initialAlgoState = AlgoState{visited = Set.empty, queue = Seq.empty}

stopWith :: (MonadError e m) => e -> m a
stopWith = throwError

(|>=) :: (MonadState s m) => ASetter' s (Seq a) -> a -> m ()
l |>= a = l %= (|> a)
