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
import Control.Monad.State (StateT (StateT), execStateT, get, modify, put)
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

type Run = StateT [Int] (Either ForthError)

toList :: ForthState -> [Int]
toList ForthState{stack} = reverse stack

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText = execStateT . evalChunk . Text.words

evalChunk :: [Text] -> Decl ()
evalChunk = \case
    ":" : name : body -> do
        when (Text.all isDigit name || last body /= ";") $
            throwError InvalidWord
        modifyEnvironment $ \environment ->
            ( Text.toLower name
            , Definition{body = init body, defEnvironment = environment}
            )
                : environment
    ops -> run $ evalOps ops
  where
    modifyEnvironment f =
        modify $ \state@ForthState{environment} ->
            state{environment = f environment}
    run action =
        StateT $ \state@ForthState{stack, environment} -> do
            stack' <- (`execStateT` stack) $ action environment
            pure ((), state{stack = stack'})

evalOps :: [Text] -> Environment -> Run ()
evalOps ops env = traverse_ (`evalOp` env) ops

evalOp :: Text -> Environment -> Run ()
evalOp op env = do
    case Text.toLower op of
        (readMaybe @Int . Text.unpack -> Just n) -> push n
        ((`lookup` env) -> Just def) -> evalDef def
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

    evalDef Definition{defEnvironment, body} = evalOps body defEnvironment

    safeDiv _ 0 = throwError DivisionByZero
    safeDiv x y = pure $ x `div` y
