{-# LANGUAGE LambdaCase #-}

module Robot (
    Bearing (East, North, South, West),
    bearing,
    coordinates,
    mkRobot,
    move,
) where

data Bearing = North | East | South | West deriving (Enum, Eq, Show)

clockWise :: Bearing -> Bearing
clockWise = \case
    West -> North
    b -> succ b

counterClockWise :: Bearing -> Bearing
counterClockWise = \case
    North -> West
    b -> pred b

data Robot = Robot {bearing :: Bearing, coordinates :: (Integer, Integer)}
    deriving (Eq, Show)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move = foldl step

step :: Robot -> Char -> Robot
step r@Robot{bearing, coordinates = (x, y)} = \case
    'A' ->
        r
            { coordinates =
                ( x + case bearing of West -> -1; East -> 1; _ -> 0
                , y + case bearing of South -> -1; North -> 1; _ -> 0
                )
            }
    'L' -> r{bearing = counterClockWise bearing}
    'R' -> r{bearing = clockWise bearing}
    instruction -> error $ show (r, instruction)
