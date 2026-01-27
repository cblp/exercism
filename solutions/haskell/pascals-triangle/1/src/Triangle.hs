module Triangle (rows) where

rows :: Int -> [[Int]]
rows 0 = []
rows 1 = [[1]]
rows n = previous ++ [newRow]
  where
    previous = rows (n - 1)
    lastRow = last previous
    newRow = 1 : zipWith (+) lastRow (tail lastRow) ++ [1]
