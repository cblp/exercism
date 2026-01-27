{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Phone (number) where

import Prelude hiding (any)

import Data.Char (isDigit)

number :: String -> Maybe String
number = match nanp . filter isDigit

nanp :: Parser
nanp = optional '1' <> area <> exch <> subs
  where
    area = ('2' ... '9') <> ptake 2
    exch = ('2' ... '9') <> ptake 2
    subs = ptake 4

-- Simple parser

newtype Parser = P (String -> Maybe (String, String))

instance Semigroup Parser where
    P p1 <> P p2 =
        P \s0 -> do
            (a1, s1) <- p1 s0
            (a2, s2) <- p2 s1
            Just (a1 <> a2, s2)

match :: Parser -> String -> Maybe String
match (P p) s =
    case p s of
        Just (a, "") -> Just a
        _ -> Nothing

psym :: (Char -> Bool) -> Parser
psym f =
    P \case
        c : cs | f c -> Just ([c], cs)
        _ -> Nothing

(...) :: Char -> Char -> Parser
a ... b = psym \c -> a <= c && c <= b

optional :: Char -> Parser
optional x =
    P \case
        c : s | c == x -> Just ("", s)
        s -> Just ("", s)

ptake :: Int -> Parser
ptake n = P $ splitAtExactly n

splitAtExactly :: Int -> String -> Maybe (String, String)
splitAtExactly n ""
    | n == 0 = Just ("", "")
    | otherwise = Nothing
splitAtExactly n s@(c : cs)
    | n < 0 = Nothing
    | n == 0 = Just ("", s)
    | otherwise = do
        (as, bs) <- splitAtExactly (n - 1) cs
        Just (c : as, bs)
