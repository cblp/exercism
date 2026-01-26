module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board =
    [ [ case cell of
            ' ' | let m = countMines r c, m > 0 -> head $ show m
            _ -> cell
      | (c, cell) <- zip [0 ..] row
      ]
    | (r, row) <- zip [0 ..] board
    ]
  where
    height = length board
    width = length $ head board
    countMines r c =
        length
            [ ()
            | i <- [max 0 (r - 1) .. min (height - 1) (r + 1)]
            , j <- [max 0 (c - 1) .. min (width - 1) (c + 1)]
            , (i, j) /= (r, c)
            , board !! i !! j == '*'
            ]
