module Queens (boardString, canAttack) where

import Data.Function ((&))
import Data.Maybe (catMaybes, fromMaybe)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString w b =
    unlines
        [ unwords [[lookup (x, y) signs & fromMaybe '_'] | y <- [0 .. 7]]
        | x <- [0 .. 7]
        ]
  where
    signs = catMaybes [(,'W') <$> w, (,'B') <$> b]

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) =
    x1 == x2 || y1 == y2 || abs (x1 - x2) == abs (y1 - y2)
