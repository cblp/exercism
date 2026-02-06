module Garden (
    Plant (..),
    garden,
    lookupPlants,
) where

import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

data Plant
    = Clover
    | Grass
    | Radishes
    | Violets
    deriving (Bounded, Enum, Eq, Show)

data Garden = Garden {students :: [String], plants :: [String]}
    deriving (Show)

garden :: [String] -> String -> Garden
garden students plants = Garden{students, plants = lines plants}

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student Garden{students, plants} = do
    i <- toList $ elemIndex student students
    map decodePlant $ concatMap (take 2 . drop (2 * i)) plants

decodePlant :: Char -> Plant
decodePlant = fromJust . (`lookup` dict)
  where
    dict = [(c, p) | p <- [minBound :: Plant ..], c : _ <- [show p]]
