module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick board =
    [ [nextState r c cell | (c, cell) <- zip [0 ..] row]
    | (r, row) <- zip [0 ..] board
    ]
  where
    height = length board

    width = length $ head board

    nextState r c cell = fromEnum $ if alive then n `div` 2 == 1 else n == 3
      where
        alive = isAlive cell
        n = countLiveNeighbors r c

    countLiveNeighbors r c =
        length
            [ ()
            | i <- [max 0 (r - 1) .. min (height - 1) (r + 1)]
            , j <- [max 0 (c - 1) .. min (width - 1) (c + 1)]
            , (i, j) /= (r, c)
            , isAlive $ board !! i !! j
            ]

    isAlive = (/= 0)
