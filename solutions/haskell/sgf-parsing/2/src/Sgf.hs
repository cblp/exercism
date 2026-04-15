{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sgf (parseSgf) where

import Debug.Trace

import Control.Applicative (many, some, (<|>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tree (Tree (Node))
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, errorBundlePretty, oneOf, runParser, token)
import Text.Megaparsec.Char (char, letterChar, upperChar)

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf =
    either
        (\err -> trace ('\n' : errorBundlePretty err) Nothing)
        (Just . pathToTree)
        . runParser @Void path "input"
  where
    path :: Parsec Void Text Path
    path = between "(" ")" $ Path <$> some step <*> many path

    step = ";" *> attrs

    attrs = Map.fromList <$> many attr

    attr = (,) <$> key <*> some (between "[" "]" string)

    key = Text.pack <$> some upperChar

    string =
        Text.pack . ($ []) . foldr (.) id
            <$> some
                ( escapedChar
                    <|> (:)
                        <$> ( letterChar
                                <|> (' ' <$ char '\t')
                                <|> oneOf ("\n[;() =" :: [Char])
                            )
                )

    escapedChar = do
        _ <- char '\\'
        token
            ( \case
                '\t' -> Just (' ' :)
                '\n' -> Just id
                c
                    | c `elem` ("]\\tn" :: [Char]) -> Just (c :)
                    | otherwise -> Nothing
            )
            Set.empty

data Path = Path {steps :: [Map Text [Text]], subpaths :: [Path]}
    deriving (Show)

pathToTree :: Path -> Tree (Map Text [Text])
pathToTree Path{steps, subpaths} = go steps
  where
    go [] = error "TODO steps :: NonEmpty"
    go [x] = Node x $ map pathToTree subpaths
    go (x : xs) = Node x [go xs]
