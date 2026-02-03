{-# LANGUAGE RecordWildCards #-}

module DND (
    Character (..),
    ability,
    modifier,
    character,
) where

import Control.Monad (replicateM)
import Data.List (sort)
import Test.QuickCheck (Gen, chooseInt)

data Character = Character
    { strength :: Int
    , dexterity :: Int
    , constitution :: Int
    , intelligence :: Int
    , wisdom :: Int
    , charisma :: Int
    , hitpoints :: Int
    }
    deriving (Show, Eq)

modifier :: Int -> Int
modifier x = x `div` 2 - 5

ability :: Gen Int
ability = sum . drop 1 . sort <$> replicateM 4 d6
  where
    d6 = chooseInt (1, 6)

character :: Gen Character
character = do
    strength <- ability
    dexterity <- ability
    constitution <- ability
    intelligence <- ability
    wisdom <- ability
    charisma <- ability
    let hitpoints = 10 + modifier constitution
    pure Character{..}
