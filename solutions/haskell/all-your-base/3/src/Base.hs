{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Base (Error (..), rebase) where

import Control.Monad (foldM, (>=>))
import Data.Ix (inRange)
import Data.List (unfoldr)
import Data.Tuple (swap)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Eq, Show)

type Result a = Either (Error a)

rebase :: (Integral a) => a -> a -> [a] -> Result a [a]
rebase inputBase outputBase = decode inputBase >=> encode outputBase

decode :: (Integral a) => a -> [a] -> Result a Integer
decode base | base < 2 = const $ Left InvalidInputBase
decode (toInteger -> base) =
    foldM
        ( \r d ->
            let di = toInteger d
            in  if inRange (0, base - 1) di then
                    Right $ r * base + di
                else
                    Left $ InvalidDigit d
        )
        0

encode :: (Integral a) => a -> Integer -> Result a [a]
encode base | base < 2 = const $ Left InvalidOutputBase
encode (toInteger -> base) =
    Right
        . map fromInteger
        . reverse
        . unfoldr \case 0 -> Nothing; m -> Just $ swap $ m `quotRem` base
