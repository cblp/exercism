{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Phone (number) where

import Control.Applicative (many, optional)
import Data.Char (isDigit)
import Data.Semigroup (stimes)
import Text.Regex.Applicative (RE, match, psym)

number :: String -> Maybe String
number = match nanp

nanp :: RE Char String
nanp =
    mconcat
        [ ignore $ many nonDigit *> optional "1" *> many nonDigit
        , digit2to9 <> digit <> digit -- area code
        , ignore $ many nonDigit
        , digit2to9 <> digit <> digit -- exchange code
        , ignore $ many nonDigit
        , stimes (4 :: Int) digit -- subscriber number
        , ignore $ many nonDigit
        ]
  where
    digit = pure <$> psym isDigit
    digit2to9 = pure <$> psym \c -> '2' <= c && c <= '9'
    nonDigit = psym $ not . isDigit
    ignore = ([] <$)
