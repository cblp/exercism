module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year =
    divisibleBy 4
    `unless`
    (   divisibleBy 100
        `unless`
        divisibleBy 400
    )
  where
    divisibleBy n = year `mod` n == 0
    unless a b = a && not b
