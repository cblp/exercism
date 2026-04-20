{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Forth (
    ForthError (..),
    evalText,
    toList,
    emptyState,
) where

import Data.Char (isDigit)
import Data.Foldable (foldlM)
import Data.Foldable qualified as Foldable
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

data ForthError
    = DivisionByZero
    | StackUnderflow
    | InvalidWord
    | UnknownWord Text
    deriving (Eq, Show)

type Run = [Int] -> Either ForthError [Int]

type Environment = [(Text, Run)]

data ForthState = ForthState {stack :: [Int], environment :: Environment}

emptyState :: ForthState
emptyState = ForthState{stack = [], environment = []}

toList :: ForthState -> [Int]
toList ForthState{stack} = reverse stack

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText = evalChunk . Text.words

evalChunk :: [Text] -> ForthState -> Either ForthError ForthState
evalChunk chunk state@ForthState{environment, stack} =
    case chunk of
        ":" : name : (unsnoc -> Just (body, ";")) ->
            addDefinition name (Foldable.toList body) state
        _ -> do
            stack' <- evalOps chunk environment stack
            Right state{stack = stack'}
  where
    -- backported from base-4.19
    unsnoc =
        foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

addDefinition :: Text -> [Text] -> ForthState -> Either ForthError ForthState
addDefinition name body state@ForthState{environment}
    | Text.all isDigit name = Left InvalidWord
    | otherwise =
        Right
            state
                { environment =
                    (Text.toLower name, evalOps body environment) : environment
                }

evalOps :: [Text] -> Environment -> Run
evalOps ops env stack = foldlM (\s op -> evalOp op env s) stack ops

evalOp :: Text -> Environment -> Run
evalOp op env = do
    case Text.toLower op of
        (readMaybe @Int . Text.unpack -> Just n) -> Right . (n :)
        ((`lookup` env) -> Just def) -> def
        "-" -> pop2 \x y -> [x - y]
        "*" -> pop2 \x y -> [x * y]
        "/" -> pop2' safeDiv
        "+" -> pop2 \x y -> [x + y]
        "drop" -> pop1 $ const []
        "dup" -> pop1 \x -> [x, x]
        "over" -> pop2 \x y -> [x, y, x]
        "swap" -> pop2 \x y -> [x, y]
        _ -> const $ Left $ UnknownWord op
  where
    pop1 f = \case x : s -> Right $ f x ++ s; _ -> Left StackUnderflow
    pop2 f = \case y : x : s -> Right $ f x y ++ s; _ -> Left StackUnderflow
    pop2' f = \case y : x : s -> (: s) <$> f x y; _ -> Left StackUnderflow

    safeDiv _ 0 = Left DivisionByZero
    safeDiv x y = Right $ x `div` y
