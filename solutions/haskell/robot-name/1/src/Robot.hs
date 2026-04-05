{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, liftIO, state)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Text.Printf (printf)

newtype Robot = Robot {serial :: IORef Int}
newtype RunState = RunState {counter :: Int}

initialState :: RunState
initialState = RunState{counter = 0}

postIncrementCounter :: StateT RunState IO Int
postIncrementCounter = state \s -> (s.counter, s{counter = s.counter + 1})

mkRobot :: StateT RunState IO Robot
mkRobot = do
    counter <- postIncrementCounter
    serial <- liftIO $ newIORef counter
    pure Robot{serial}

resetName :: Robot -> StateT RunState IO ()
resetName robot = do
    counter <- postIncrementCounter
    liftIO $ writeIORef robot.serial counter

robotName :: Robot -> IO String
robotName robot = printf "AA%03d" <$> readIORef robot.serial
