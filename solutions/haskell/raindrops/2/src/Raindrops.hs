{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Raindrops (convert) where

import Data.Text (Text)
import Data.Text qualified as Text

convert :: Int -> Text
convert n = (3 ? "Pling" <> 5 ? "Plang" <> 7 ? "Plong") ?: Text.pack (show n)
  where
    a ? b = if n `mod` a == 0 then b else ""
    a ?: b = case a of "" -> b; _ -> a
