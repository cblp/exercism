module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map, assocs, fromList)
import Data.Text (Text, unpack)

transform :: Map a Text -> Map Char a
transform legacyData =
    fromList
        [ (toLower letter, a)
        | (a, letters) <- assocs legacyData
        , letter <- unpack letters
        ]
