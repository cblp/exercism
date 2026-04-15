{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Sgf (parseSgf) where

import Control.Applicative (empty, many, some, (<|>))
import Control.Monad.State (evalStateT, get, guard, put)
import Data.Char (isLetter, isUpper)
import Data.Map (Map, fromList)
import Data.Text (Text, pack)
import Data.Tree (Tree (Node))

parseSgf :: String -> Maybe (Tree (Map Text [Text]))
parseSgf = evalStateT $ path <* eof
  where
    -- helpers
    check p = get >>= \case c : cs | p c -> put cs >> pure c; _ -> empty
    char c = check (== c)
    eof = get >>= guard . null
    -- grammar
    path = do
        steps <- char '(' *> some (char ';' >> fromList <$> many attr)
        subs <- many path <* char ')'
        pure $ foldr (\s t -> Node s [t]) (Node (last steps) subs) (init steps)
    attr =
        ((,) . pack <$> some (check isUpper))
            <*> some (char '[' *> string <* char ']')
    string = pack . foldr id [] <$> some (escaped <|> (:) <$> simple)
    escaped = do
        c <- char '\\' *> check (`elem` "\t\n]\\tn")
        pure case c of '\t' -> (' ' :); '\n' -> id; _ -> (c :)
    simple =
        check isLetter <|> ' ' <$ check (== '\t') <|> check (`elem` "\n[;() =")
