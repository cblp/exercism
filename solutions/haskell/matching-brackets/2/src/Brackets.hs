{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Brackets (arePaired) where

import Data.List.Extra (list)

arePaired :: String -> Bool
arePaired = (`go` [])
  where
    go = \case
        [] -> null
        x : xs
            | x `elem` opening -> \bs -> go xs (x : bs)
            | Just b' <- open x -> list False \b bs -> b == b' && go xs bs
            | otherwise -> go xs

    opening = "([{"
    closing = ")]}"
    open = (`lookup` zip closing opening)
