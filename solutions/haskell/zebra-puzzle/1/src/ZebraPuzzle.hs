{-# LANGUAGE BlockArguments #-}

module ZebraPuzzle (Resident (..), Solution (..), solve) where

import Control.Monad (guard)
import Data.List (permutations, zip5)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
    deriving (Bounded, Enum, Eq, Show)

data Color = Red | Green | White | Yellow | Blue
    deriving (Bounded, Enum, Eq, Show)

data Pet = Dog | Snail | Fox | Horse | Zebra
    deriving (Bounded, Enum, Eq, Show)

data Drink = Tea | Coffee | Milk | Juice | Water
    deriving (Bounded, Enum, Eq, Show)

data Hobby = Dance | Paint | Read | Football | Chess
    deriving (Bounded, Enum, Eq, Show)

data Solution = Solution
    { waterDrinker :: Resident
    , zebraOwner :: Resident
    }
    deriving (Eq, Show)

solve :: Solution
solve = Solution{waterDrinker, zebraOwner}
  where
    waterDrinker =
        head [resident | (resident, _, _, Water, _) <- concat solutions]
    zebraOwner =
        head [resident | (resident, _, Zebra, _, _) <- concat solutions]

solutions :: [[(Resident, Color, Pet, Drink, Hobby)]]
solutions = do
    color <- values
    leftOf color Green color White -- 6
    resident <- values
    first resident Norwegian -- 10
    same resident Englishman color Red -- 2
    nextTo resident Norwegian color Blue -- 15
    drink <- values
    middle drink Milk -- 9
    same drink Coffee color Green -- 4
    same resident Ukrainian drink Tea -- 5
    pet <- values
    same resident Spaniard pet Dog -- 3
    hobby <- values
    same hobby Dance pet Snail -- 7
    same color Yellow hobby Paint -- 8
    nextTo hobby Read pet Fox -- 11
    nextTo hobby Paint pet Horse -- 12
    same hobby Football drink Juice -- 13
    same resident Japanese hobby Chess -- 14
    pure $ zip5 resident color pet drink hobby
  where
    same_ xs x ys y = (x, y) `elem` zip xs ys
    leftOf_ xs x ys = same_ xs x (tail ys)

    same xs x ys y = guard $ same_ xs x ys y
    leftOf xs x ys y = guard $ leftOf_ xs x ys y
    nextTo xs x ys y = guard $ leftOf_ xs x ys y || leftOf_ ys y xs x
    middle xs x = guard $ xs !! 2 == x
    first xs x = guard $ head xs == x

values :: (Bounded a, Enum a) => [[a]]
values = permutations [minBound .. maxBound]
