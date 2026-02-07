{-# LANGUAGE LambdaCase #-}

module Isogram (isIsogram) where

import Data.Char (isLetter)
import Data.Text (Text, toLower, unpack)

isIsogram :: Text -> Bool
isIsogram = go . filter isLetter . unpack . toLower
  where
    go = \case
        [] -> True
        x : xs -> x `notElem` xs && go xs
