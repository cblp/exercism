{-# LANGUAGE LambdaCase #-}

module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map recite1 [start .. stop]
  where
    recite1 n =
        "On the "
            ++ ordinals !! (n - 1)
            ++ " day of Christmas my true love gave to me: "
            ++ commasAnd (reverse $ take n gifts)
            ++ "."

    ordinals =
        [ "first"
        , "second"
        , "third"
        , "fourth"
        , "fifth"
        , "sixth"
        , "seventh"
        , "eighth"
        , "ninth"
        , "tenth"
        , "eleventh"
        , "twelfth"
        ]

    gifts =
        [ "a Partridge in a Pear Tree"
        , "two Turtle Doves"
        , "three French Hens"
        , "four Calling Birds"
        , "five Gold Rings"
        , "six Geese-a-Laying"
        , "seven Swans-a-Swimming"
        , "eight Maids-a-Milking"
        , "nine Ladies Dancing"
        , "ten Lords-a-Leaping"
        , "eleven Pipers Piping"
        , "twelve Drummers Drumming"
        ]

    commasAnd = \case
        [] -> undefined
        [a] -> a
        [a, b] -> a ++ ", and " ++ b
        a : bs -> a ++ ", " ++ commasAnd bs
