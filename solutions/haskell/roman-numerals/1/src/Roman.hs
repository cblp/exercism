module Roman (numerals) where

numerals :: Int -> Maybe String
numerals n
    | 0 < n, n < 4000 = Just $ go romanDigits "" n
    | otherwise = Nothing
  where
    go [] out _ = out
    go rds@((value, symbol) : rds') out m
        | m >= value = go rds (out <> symbol) (m - value)
        | otherwise = go rds' out m

romanDigits :: [(Int, String)]
romanDigits =
    concat
        [ [(1000, "M")]
        , [(900, "CM"), (500, "D"), (400, "CD"), (100, "C")]
        , [(90, "XC"), (50, "L"), (40, "XL"), (10, "X")]
        , [(9, "IX"), (5, "V"), (4, "IV"), (1, "I")]
        ]
