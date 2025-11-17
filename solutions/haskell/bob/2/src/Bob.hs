{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import Data.Char (isLetter, isSpace, isUpper)
import Data.Text (Text, isSuffixOf, strip)
import Data.Text qualified as Text

responseFor :: Text -> Text
responseFor input_raw
    | is_silence = "Fine. Be that way!"
    | is_question && is_yelling = "Calm down, I know what I'm doing!"
    | is_question = "Sure."
    | is_yelling = "Whoa, chill out!"
    | otherwise = "Whatever."
  where
    input = strip input_raw
    is_silence = Text.all isSpace input
    is_question = "?" `isSuffixOf` input
    letters = Text.filter isLetter input
    is_yelling = not (Text.null letters) && Text.all isUpper letters
