{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Forth (
    ForthError (..),
    evalText,
    toList,
    emptyState,
) where

import Control.Lens (makeLenses, use, view, (%=), (.=))
import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT, execStateT)
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

data Definition = Definition {body :: [Text], environment :: Environment}
    deriving (Show)

type Environment = [(Text, Definition)]

data ForthState = ForthState {_stack :: [Int], _environment :: Environment}
    deriving (Show)
makeLenses ''ForthState

emptyState :: ForthState
emptyState = ForthState{_stack = [], _environment = []}

type Forth = StateT ForthState (Either ForthError)

toList :: ForthState -> [Int]
toList = reverse . view stack

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText = execStateT . evalChunk . Text.words

evalChunk :: [Text] -> Forth ()
evalChunk = \case
    ":" : name : body -> do
        when (Text.all isDigit name) $ throwError InvalidWord
        env <- use environment
        environment
            %= ( ( Text.toLower name
                 , Definition{body = init body, environment = env}
                 )
                    :
               )
    ops -> evalOps ops

evalOps :: [Text] -> Forth ()
evalOps = traverse_ evalOp

evalOp :: Text -> Forth ()
evalOp op = do
    env <- use environment
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
    push x = stack %= (x :)

    pop =
        use stack >>= \case
            x : xs -> do stack .= xs; pure x
            _ -> throwError StackUnderflow

    lookTop =
        use stack >>= \case x : _ -> pure x; _ -> throwError StackUnderflow

    lookOver =
        use stack >>= \case _ : x : _ -> pure x; _ -> throwError StackUnderflow

    evalDef def = do
        outerEnvironment <- use environment
        environment .= def.environment
        evalOps def.body
        environment .= outerEnvironment

safeDiv :: Int -> Int -> Forth Int
safeDiv _ 0 = throwError DivisionByZero
safeDiv x y = pure $ x `div` y
