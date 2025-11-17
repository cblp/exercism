module LeapYear (isLeapYear) where

import Control.Applicative (liftA2)

isLeapYear :: Integer -> Bool
isLeapYear = divisibleBy 4 &! (divisibleBy 100 &! divisibleBy 400)
  where
    divisibleBy n year = year `mod` n == 0
    (&!) = liftA2 (>) -- \a b x = a x && not (b x)
