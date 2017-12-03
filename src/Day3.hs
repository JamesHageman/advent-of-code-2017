module Day3 where

spiral :: [(Int, Int)]
spiral = spiralFrom (0, 0)

spiralFrom :: (Int, Int) -> [(Int, Int)]
spiralFrom curr = curr : (spiralFrom $ nextPosition curr)

nextPosition :: (Int, Int) -> (Int, Int)
nextPosition (x, y)
  | x > 0 && -x < y && y < x  = (x, y + 1)
  | y > 0 && -y < x && x <= y = (x - 1, y)
  | x < 0 && x < y && y <= -x = (x, y - 1)
  | otherwise = (x + 1, y)

positionOf :: Int -> (Int, Int)
positionOf i = spiral !! (i - 1)

manhattanDistance (x, y) = abs x + abs y

day3part1 = manhattanDistance $ positionOf 325489

test =
  [
    (0, 0) == positionOf 1,
    (2, 1) == positionOf 12,
    (0, -2) == positionOf 23,
    (1, -2) == positionOf 24,
    (2,-2) == positionOf 25,
    (3,-2) == positionOf 26,
    (3,-1) == positionOf 27
  ]
