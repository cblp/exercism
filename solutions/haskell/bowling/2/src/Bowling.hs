{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bowling (score, BowlingError (..)) where

import Control.Arrow
import Control.Monad

data BowlingError
    = IncompleteGame
    | InvalidRoll {rollIndex :: Int, rollValue :: Int}
    deriving (Eq, Show)

data Bonus = NoBonus | Spare | Strike deriving (Eq, Show)

data Game = Game
    { frames :: Int
    , throws :: Int
    -- ^ Throws made in the current frame
    , pinsUp :: Int
    , points :: Int
    , lastBonuses :: (Bonus, Bonus)
    , ended :: Bool
    }
    deriving (Show)

score :: [Int] -> Either BowlingError Int
score = zip [0 ..] >>> foldM roll start >=> end
  where
    start =
        Game
            { frames = 0
            , throws = 0
            , pinsUp = 10
            , points = 0
            , lastBonuses = (NoBonus, NoBonus)
            , ended = False
            }

    end :: Game -> Either BowlingError Int
    end game
        | game.ended = Right game.points
        | otherwise = Left IncompleteGame

    roll :: Game -> (Int, Int) -> Either BowlingError Game
    roll game (rollIndex, pins)
        | game.ended || pins < 0 || pins > game.pinsUp =
            Left
                InvalidRoll
                    { rollIndex
                    , rollValue = pins
                    }
        | otherwise =
            let (bonus1, bonus2) = game.lastBonuses
                frame = game.frames + 1
                throw = game.throws + 1
                pinsLeft = game.pinsUp - pins
                points =
                    game.points
                        + pins
                            * flags
                                [ frame <= 10
                                , bonus2 == Spare || bonus2 == Strike
                                , bonus1 == Strike
                                ]
                bonus
                    | frame > 10 || pinsLeft /= 0 = NoBonus
                    | throw == 1 = Strike
                    | otherwise = Spare
                ended =
                    frame == 10 && throw == 2 && pinsLeft > 0
                        || frame == 11 && throw == 2 && bonus1 == Strike
                        || frame == 11 && bonus2 == Spare
                        || frame == 12
                (frames, throws, pinsUp)
                    | pinsLeft == 0 || throw == 2 = (game.frames + 1, 0, 10)
                    | otherwise = (game.frames, throw, pinsLeft)
             in Right
                    Game
                        { frames
                        , throws
                        , pinsUp
                        , points
                        , ended
                        , lastBonuses = (bonus2, bonus)
                        }

    flags = sum . map fromEnum
