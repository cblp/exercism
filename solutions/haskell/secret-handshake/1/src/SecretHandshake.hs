{-# LANGUAGE BinaryLiterals #-}

module SecretHandshake (handshake) where

import Data.Bits (testBit)
import Data.Function ((&))

handshake :: Int -> [String]
handshake n =
    [a | (i, a) <- zip [0 ..] actions, bit i] & if bit 4 then reverse else id
  where
    bit = testBit n
    actions = ["wink", "double blink", "close your eyes", "jump"]
