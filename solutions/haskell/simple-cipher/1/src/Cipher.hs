module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Control.Monad (replicateM)
import Data.Char (chr, ord)
import System.Random (randomRIO)

caesarDecode :: String -> String -> String
caesarDecode = zipWith (shift . negate . letterToInt) . cycle

caesarEncode :: String -> String -> String
caesarEncode = zipWith (shift . letterToInt) . cycle

shift :: Int -> Char -> Char
shift k c = intToLetter $ (letterToInt c + k) `mod` 26

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    key <- replicateM 100 $ randomRIO ('a', 'z')
    pure (key, caesarEncode key text)

letterToInt :: Char -> Int
letterToInt = subtract (ord 'a') . ord

intToLetter :: Int -> Char
intToLetter = chr . (+ ord 'a')
