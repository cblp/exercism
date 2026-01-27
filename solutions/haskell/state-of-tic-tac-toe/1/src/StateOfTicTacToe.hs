module StateOfTicTacToe (gameState, GameState (..)) where

import Data.List (transpose)

{-# ANN module ("HLint: ignore Use head" :: String) #-}

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
    | not (countX == countO || countX == countO + 1) = Impossible
    | anyLineX && anyLineO = Impossible
    | anyLineX = WinX
    | anyLineO = WinO
    | allFilled = Draw
    | otherwise = Ongoing
  where
    rows = board
    cols = transpose board
    diag1 = [board !! 0 !! 0, board !! 1 !! 1, board !! 2 !! 2]
    diag2 = [board !! 0 !! 2, board !! 1 !! 1, board !! 2 !! 0]
    allFilled = ' ' `notElem` concat board
    anyLine s =
        any (all (== s)) rows
            || any (all (== s)) cols
            || all (== s) diag1
            || all (== s) diag2
    anyLineX = anyLine 'X'
    anyLineO = anyLine 'O'
    countX = length $ concatMap (filter (== 'X')) board
    countO = length $ concatMap (filter (== 'O')) board
