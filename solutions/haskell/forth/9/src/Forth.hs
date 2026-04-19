{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Forth (
    ForthError (..),
    evalText,
    toList,
    emptyState,
) where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, execStateT, get, modify, put)
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Foldable qualified as Foldable
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

data ForthError
    = DivisionByZero
    | StackUnderflow
    | InvalidWord
    | UnknownWord Text
    deriving (Eq, Show)

type Run = StateT [Int] (Either ForthError)

type Environment = [(Text, Run ())]

data ForthState = ForthState {stack :: [Int], environment :: Environment}

emptyState :: ForthState
emptyState = ForthState{stack = [], environment = []}

toList :: ForthState -> [Int]
toList ForthState{stack} = reverse stack

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText = evalChunk . Text.words

evalChunk :: [Text] -> ForthState -> Either ForthError ForthState
evalChunk chunk state@ForthState{environment, stack} =
    case Seq.fromList chunk of
        ":" :<| name :<| (body :|> ";") ->
            addDefinition name (Foldable.toList body) state
        _ -> do
            stack' <- (`execStateT` stack) $ evalOps chunk environment
            Right state{stack = stack'}

addDefinition :: Text -> [Text] -> ForthState -> Either ForthError ForthState
addDefinition name body state@ForthState{environment}
    | Text.all isDigit name = Left InvalidWord
    | otherwise =
        Right
            state
                { environment =
                    ( Text.toLower name
                    , evalOps body environment
                    )
                        : environment
                }

evalOps :: [Text] -> Environment -> Run ()
evalOps ops env = traverse_ (`evalOp` env) ops

evalOp :: Text -> Environment -> Run ()
evalOp op env = do
    case Text.toLower op of
        (readMaybe @Int . Text.unpack -> Just n) -> push n
        ((`lookup` env) -> Just def) -> def
        "+" -> do y <- pop; x <- pop; push $ x + y
        "-" -> do y <- pop; x <- pop; push $ x - y
        "*" -> do y <- pop; x <- pop; push $ x * y
        "/" -> do y <- pop; x <- pop; push =<< x `safeDiv` y
        "dup" -> do x <- pop; push x; push x
        "drop" -> void pop
        "swap" -> do x <- pop; y <- pop; push x; push y
        "over" -> lookOver >>= push
        _ -> throwError $ UnknownWord op
  where
    push x = modify (x :)

    pop =
        get >>= \case
            x : xs -> do put xs; pure x
            _ -> throwError StackUnderflow

    lookOver =
        get >>= \case
            _ : x : _ -> pure x
            _ -> throwError StackUnderflow

    safeDiv _ 0 = throwError DivisionByZero
    safeDiv x y = pure $ x `div` y
