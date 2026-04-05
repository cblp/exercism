module Say (inEnglish) where

import Control.Monad.State (evalState, state)
import Data.Char (digitToInt)

inEnglish :: Integer -> Maybe String
inEnglish n
    | n < 0 = Nothing
    | n == 0 = Just "zero"
    | otherwise =
        Just $ unwords $ reverse $ sayParts $ map digitToInt $ reverse $ show n

sayParts :: [Int] -> [String]
sayParts digits = flip evalState digits $ do
    groupOnes <- state $ splitAt 3
    groupThousands <- state $ splitAt 3
    groupMillions <- state $ splitAt 3
    groupBillions <- state $ splitAt 3
    pure . concat $
        sayGroup groupOnes
            : sayGroup' "thousand" groupThousands
            ++ sayGroup' "million" groupMillions
            ++ sayGroup' "billion" groupBillions
  where
    sayGroup' name group = [name : sayGroup group | any (/= 0) group]

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
