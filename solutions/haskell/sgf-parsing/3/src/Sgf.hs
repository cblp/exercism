{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sgf (parseSgf) where

import Control.Applicative (many, some, (<|>))
import Data.List.NonEmpty (NonEmpty ((:|)), some1)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tree (Tree (Node))
import Data.Void (Void)
import Text.Megaparsec (between, oneOf, parseMaybe, token)
import Text.Megaparsec.Char (char, letterChar, upperChar)

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf = fmap pathToTree . parseMaybe @Void path
  where
    path = between "(" ")" $ Path <$> some1 step <*> many path

    step = ";" *> attrs

    attrs = Map.fromList <$> many attr

    attr = (,) <$> key <*> some (between "[" "]" string)

    key = Text.pack <$> some upperChar

    string = do
        charPrependers <- some $ escapedChar <|> (:) <$> simpleChar
        pure $ Text.pack $ foldr id [] charPrependers

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

    simpleChar =
        letterChar <|> (' ' <$ char '\t') <|> oneOf ("\n[;() =" :: [Char])

data Path = Path {steps :: NonEmpty (Map Text [Text]), subpaths :: [Path]}
    deriving (Show)

pathToTree :: Path -> Tree (Map Text [Text])
pathToTree Path{steps = step :| steps, subpaths} = go step steps
  where
    go x =
        Node x
            . \case
                [] -> map pathToTree subpaths
                y : ys -> [go y ys]
