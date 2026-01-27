{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Phone (number) where

import Data.Char (isDigit)
import Data.Semigroup (stimes)

number :: String -> Maybe String
number = match nanp

nanp :: Parser
nanp =
    mconcat
        [ skipUntil isDigit <> skipOptional '1' <> skipUntil isDigit
        , ('2' ... '9') <> digit <> digit -- area code
        , skipUntil isDigit
        , ('2' ... '9') <> digit <> digit -- exchange code
        , skipUntil isDigit
        , stimes (4 :: Int) digit -- subscriber number
        , skipUntil isDigit
        ]
  where
    digit = psym isDigit

-- Simple parser

newtype Parser = P (String -> Maybe (String, String))

instance Semigroup Parser where
    P p1 <> P p2 =
        P \s0 -> do
            (a1, s1) <- p1 s0
            (a2, s2) <- p2 s1
            Just (a1 <> a2, s2)

instance Monoid Parser where
    mempty = P \s -> Just ("", s)

match :: Parser -> String -> Maybe String
match (P p) s = case p s of
    Just (a, "") -> Just a
    _ -> Nothing

psym :: (Char -> Bool) -> Parser
psym f = P \case
    c : cs | f c -> Just ([c], cs)
    _ -> Nothing

skipUntil :: (Char -> Bool) -> Parser
skipUntil f = P \s -> Just (mempty, dropWhile (not . f) s)

(...) :: Char -> Char -> Parser
a ... b = psym \c -> a <= c && c <= b

skipOptional :: Char -> Parser
skipOptional x = P \case
    c : s | c == x -> Just (mempty, s)
    s -> Just (mempty, s)
