{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Alphametics (solve) where

import Data.Char (isAlpha)
import Data.Foldable (toList)
import Data.List (foldl', (\\))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type Equation = NonEmpty Char

solve :: String -> Maybe [(Char, Int)]
solve puzzle =
    fmap Map.assocs $ dedup $ go 0 Map.empty $ makeEquations parts
  where
    parts = parse puzzle

    leadingVars :: Set Char
    leadingVars = Set.fromList $ map last $ toList parts

    go :: Int -> Map Char Int -> [Equation] -> [Map Char Int]
    go carry env = \case
        []
            | carry == 0 -> [env]
            | otherwise -> undefined
        eq : eqs -> do
            (carry', env') <- trySolve eq carry env
            go carry' env' eqs

    dedup :: (Show a) => [a] -> Maybe a
    dedup = \case
        [] -> Nothing
        [a] -> Just a
        a : b : _ ->
            error $ "too many solutions: " ++ show a ++ ", " ++ show b ++ "..."

    trySolve ::
        NonEmpty Char ->
        Int ->
        Map Char Int ->
        -- \| Variants of carry with modified env
        [(Int, Map Char Int)]
    trySolve (resultVar :| summands) carry env =
        [ (resultCarry, env'')
        | (result, env') <- apply summands carry env
        , let (resultCarry, resultDigit) = divMod result 10
        , env'' <-
            case Map.lookup resultVar env' of
                Nothing ->
                    [ Map.insert resultVar resultDigit env'
                    | resultDigit `notElem` env'
                    , resultVar `notElem` leadingVars || resultDigit /= 0
                    ]
                Just resultDigit' -> [env' | resultDigit == resultDigit']
        ]

    apply ::
        [Char] ->
        Int ->
        Map Char Int ->
        -- \| Variants of result with modified env
        [(Int, Map Char Int)]
    apply [] carry env = [(carry, env)]
    apply (var : vars) carry env
        | Just value <- Map.lookup var env = apply vars (value + carry) env
        | otherwise = do
            value <- freeDigits
            apply vars (value + carry) (Map.insert var value env)
      where
        freeDigits =
            (if var `elem` leadingVars then [1 .. 9] else [0 .. 9])
                \\ Map.elems env

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

makeEquation :: Char -> [String] -> (Equation, [String])
makeEquation c ss = (c :| [a | a : _ <- ss], [as | _ : as <- ss])
