{-# LANGUAGE ParallelListComp #-}

module House (rhyme) where

import Data.List (intercalate, intersperse, tails)

rhyme :: String
rhyme = unlines $ intersperse "" verses
  where
    verses = ["This is the " ++ body ++ "." | body <- bodies]

    bodies =
        [ intercalate "\nthat " $ character : middleLine
        | character <- characters
        | middleLine <- middleLines
        ]

    characters =
        [ "house that Jack built"
        , "malt"
        , "rat"
        , "cat"
        , "dog"
        , "cow with the crumpled horn"
        , "maiden all forlorn"
        , "man all tattered and torn"
        , "priest all shaven and shorn"
        , "rooster that crowed in the morn"
        , "farmer sowing his corn"
        , "horse and the hound and the horn"
        ]

    middleLines =
        reverse $
            tails
                [ relation ++ " the " ++ character
                | relation <- relations
                | character <- tail (reverse characters)
                ]

    relations =
        [ "belonged to"
        , "kept"
        , "woke"
        , "married"
        , "kissed"
        , "milked"
        , "tossed"
        , "worried"
        , "killed"
        , "ate"
        , "lay in"
        ]
