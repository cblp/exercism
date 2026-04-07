{-# LANGUAGE LambdaCase #-}

module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go []
  where
    go [] [] = True
    go (_ : _) [] = False
    go bracketStack (x : xs)
        | isOpen x = go (x : bracketStack) xs
        | Just y <- getOpenIfClose x =
            case bracketStack of
                [] -> False
                b : bs -> b == y && go bs xs
        | otherwise = go bracketStack xs

    isOpen = (`elem` "([{")

    getOpenIfClose = \case
        ')' -> Just '('
        ']' -> Just '['
        '}' -> Just '{'
        _ -> Nothing
