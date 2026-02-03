module ResistorColors (Color (..), Resistor (..), label, ohms) where

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
    deriving (Show, Enum, Bounded)

newtype Resistor = Resistor {bands :: (Color, Color, Color)}
    deriving (Show)

label :: Resistor -> String
label (Resistor (a, b, c)) =
    mconcat
        [ case d `mod` 3 of
            0 -> show n
            1 -> show n <> "0"
            _ -> show (n `div` 10) <> "." <> show (n `mod` 10)
        , " "
        , ["", "kilo", "mega", "giga"] !! ((d + 1) `div` 3)
        , "ohms"
        ]
  where
    (n, d) =
        case (b, c) of
            (Black, Black) -> (0, 0)
            (Black, _) -> (fromEnum a, 1 + fromEnum c)
            _ -> (fromEnum a * 10 + fromEnum b, fromEnum c)

ohms :: Resistor -> Int
ohms (Resistor (a, b, c)) = (fromEnum a * 10 + fromEnum b) * 10 ^ fromEnum c
