module Say (inEnglish) where

import Data.Char (digitToInt)

inEnglish :: Integer -> Maybe String
inEnglish n
    | n < 0 = Nothing
    | n == 0 = Just "zero"
    | otherwise =
        Just $ unwords $ reverse $ sayParts $ map digitToInt $ reverse $ show n

sayParts :: [Int] -> [String]
sayParts digits =
    sayGroup groupOnes
        ++ concat
            [ "thousand" : sayGroup groupThousands
            | length digits > 3
            , any (/= 0) groupThousands
            ]
        ++ concat
            [ "million" : sayGroup groupMillions
            | length digits > 6
            , any (/= 0) groupMillions
            ]
        ++ concat
            [ "billion" : sayGroup groupBillions
            | length digits > 9
            , any (/= 0) groupBillions
            ]
  where
    groupOnes = take 3 digits
    groupThousands = drop 3 $ take 6 digits
    groupMillions = drop 6 $ take 9 digits
    groupBillions = drop 9 $ take 12 digits

sayGroup :: [Int] -> [String]
sayGroup digits_ =
    ( case digits of
        0 : 0 : _ -> []
        0 : b : _ -> [tens !! (b - 1)]
        a : 0 : _ -> [ones !! (a - 1)]
        a : 1 : _ -> [teens !! (a - 1)]
        a : b : _ -> [tens !! (b - 1) <> "-" <> ones !! (a - 1)]
        _ -> []
    )
        ++ case digits of
            [_, _, c] | c /= 0 -> ["hundred", ones !! (c - 1)]
            _ -> []
  where
    digits = digits_ ++ replicate (3 - length digits_) 0

ones :: [String]
ones = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

tens :: [String]
tens =
    [ "ten"
    , "twenty"
    , "thirty"
    , "forty"
    , "fifty"
    , "sixty"
    , "seventy"
    , "eighty"
    , "ninety"
    ]

teens :: [String]
teens =
    [ "eleven"
    , "twelve"
    , "thirteen"
    , "fourteen"
    , "fifteen"
    , "sixteen"
    , "seventeen"
    , "eighteen"
    , "nineteen"
    ]
