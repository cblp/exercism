module Bob (responseFor) where

import Data.Char (isLetter, isSpace, isUpper)
import Data.List (isSuffixOf)

responseFor :: String -> String
responseFor input_raw
    | is_silence = "Fine. Be that way!"
    | is_question && is_yelling = "Calm down, I know what I'm doing!"
    | is_question = "Sure."
    | is_yelling = "Whoa, chill out!"
    | otherwise = "Whatever."
  where
    input = trim input_raw
    is_silence = all isSpace input
    is_question = "?" `isSuffixOf` input
    letters = filter isLetter input
    is_yelling = not (null letters) && all isUpper letters

trim :: String -> String
trim = reverse . trim_prefix . reverse . trim_prefix
  where
    trim_prefix = dropWhile isSpace
