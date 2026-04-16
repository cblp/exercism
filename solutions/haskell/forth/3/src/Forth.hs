{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Forth (
    ForthError (..),
    evalText,
    toList,
    emptyState,
) where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State.Strict (
    StateT (StateT),
    execStateT,
    get,
    modify,
    put,
 )
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

data ForthError
    = DivisionByZero
    | StackUnderflow
    | InvalidWord
    | UnknownWord Text
    deriving (Eq, Show)

data Definition = Definition {body :: [Text], defEnvironment :: Environment}
    deriving (Show)

type Environment = [(Text, Definition)]

data ForthState = ForthState {stack :: [Int], environment :: Environment}
    deriving (Show)

emptyState :: ForthState
emptyState = ForthState{stack = [], environment = []}

type Decl = StateT ForthState (Either ForthError)

type Run = ReaderT Environment (StateT [Int] (Either ForthError))

toList :: ForthState -> [Int]
toList ForthState{stack} = reverse stack

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText = execStateT . evalChunk . Text.words

evalChunk :: [Text] -> Decl ()
evalChunk = \case
    ":" : name : body -> do
        when (Text.all isDigit name) $ throwError InvalidWord
        modify $ \state@ForthState{environment} ->
            state
                { environment =
                    ( Text.toLower name
                    , Definition{body = init body, defEnvironment = environment}
                    )
                        : environment
                }
    ops ->
        StateT $ \state@ForthState{stack, environment} -> do
            stack' <-
                (`execStateT` stack) $ (`runReaderT` environment) $ evalOps ops
            pure ((), state{stack = stack'})

evalOps :: [Text] -> Run ()
evalOps = traverse_ evalOp

evalOp :: Text -> Run ()
evalOp op = do
    env <- ask
    case Text.toLower op of
        (readMaybe @Int . Text.unpack -> Just n) -> push n
        ((`lookup` env) -> Just def) -> evalDef def
        "+" -> do x <- pop; y <- pop; push $ y + x
        "-" -> do x <- pop; y <- pop; push $ y - x
        "*" -> do x <- pop; y <- pop; push $ y * x
        "/" -> do x <- pop; y <- pop; push =<< y `safeDiv` x
        "dup" -> lookTop >>= push
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

    lookTop = get >>= \case x : _ -> pure x; _ -> throwError StackUnderflow

    lookOver = get >>= \case _ : x : _ -> pure x; _ -> throwError StackUnderflow

    evalDef Definition{defEnvironment, body} =
        local (const defEnvironment) $ evalOps body

safeDiv :: Int -> Int -> Run Int
safeDiv _ 0 = throwError DivisionByZero
safeDiv x y = pure $ x `div` y
