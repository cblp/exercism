module Meetup (Weekday (..), Schedule (..), meetupDay) where

import Data.Time

data Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Enum)

data Schedule
    = First
    | Second
    | Third
    | Fourth
    | Last
    | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> MonthOfYear -> Day
meetupDay schedule weekday year month =
    select schedule . filter ((== weekday') . dayOfWeek) $ makeMonth year month
  where
    weekday' = toEnum $ fromEnum weekday + 1

makeMonth :: Integer -> MonthOfYear -> [Day]
makeMonth year month =
    [ fromGregorian year month 1
    .. fromGregorian year month (gregorianMonthLength year month)
    ]

select :: Schedule -> [Day] -> Day
select First (x : _) = x
select Second (_ : x : _) = x
select Third (_ : _ : x : _) = x
select Fourth (_ : _ : _ : x : _) = x
select Last xs = last xs
select Teenth xs = head $ filter isTeenth xs
select _ _ = error "Invalid schedule or weekday"

isTeenth :: Day -> Bool
isTeenth day =
    let (_, _, d) = toGregorian day
    in  d >= 13 && d <= 19
