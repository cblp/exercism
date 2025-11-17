{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Alphametics (solve) where

import Data.Char (isAlpha)
import Data.Foldable (toList)
import Data.List (foldl', (\\))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (fromMaybe)

type Equation = NonEmpty Char

solve :: String -> Maybe [(Char, Int)]
solve puzzle = dedup $ go 0 [] $ makeEquations parts
  where
    parts = parse puzzle

    leadingVars = map last $ toList parts

    go carry env = \case
        []
            | carry == 0 -> [env]
            | otherwise -> undefined
        eq : eqs -> do
            (carry', env') <- trySolve eq carry env
            go carry' env' eqs

    trySolve (resultVar :| summands) carry env =
        [ (resultCarry, env'')
        | (result, env') <- apply carry env summands
        , let (resultCarry, resultDigit) = divMod result 10
        , env'' <-
            case lookup resultVar env' of
                Nothing ->
                    [ (resultVar, resultDigit) : env'
                    | resultDigit `notElem` map snd env'
                    , resultVar `notElem` leadingVars || resultDigit /= 0
                    ]
                Just resultDigit' -> [env' | resultDigit == resultDigit']
        ]

    apply carry env = \case
        [] -> [(carry, env)]
        var : vars
            | Just value <- lookup var env -> apply (value + carry) env vars
            | otherwise -> do
                value <- freeDigits
                apply (value + carry) ((var, value) : env) vars
          where
            freeDigits =
                (if var `elem` leadingVars then [1 .. 9] else [0 .. 9])
                    \\ map snd env

dedup :: (Show a) => [a] -> Maybe a
dedup = \case
    [] -> Nothing
    [a] -> Just a
    a : b : _ ->
        error $ "too many solutions: " ++ show a ++ ", " ++ show b ++ "..."

parse :: String -> NonEmpty String
parse = fromMaybe undefined . nonEmpty . fin . foldl' go ("", [])
  where
    go s@(wordBuffer, resWords) c
        | isAlpha c = (c : wordBuffer, resWords)
        | otherwise = ([], fin s)
    fin = \case
        ("", resWords) -> resWords
        (wordBuffer, resWords) -> wordBuffer : resWords

makeEquations :: NonEmpty String -> [Equation]
makeEquations = \case
    "" :| _ -> []
    (r : rs) :| ss ->
        let (e, ss') = makeEquation r ss in e : makeEquations (rs :| ss')
  where
    makeEquation c ss = (c :| [a | a : _ <- ss], [as | _ : as <- ss])
