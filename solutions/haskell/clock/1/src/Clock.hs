module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

newtype Clock = Minutes Int
    deriving (Eq)

dayMinutes :: Int
dayMinutes = 24 * 60

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Minutes $ (h * 60 + m) `mod` dayMinutes

toString :: Clock -> String
toString (Minutes minutes) = printf "%02d:%02d" h m
  where
    (h, m) = minutes `divMod` 60

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m (Minutes m') = fromHourMin h (m + m')
