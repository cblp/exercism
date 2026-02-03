module ResistorColors (Color (..), value) where

data Color
    = Black
    | Brown
    | Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Violet
    | Grey
    | White
    deriving (Bounded, Enum, Eq, Ord, Show)

value :: (Color, Color) -> Int
value (a, b) = fromEnum a * 10 + fromEnum b
