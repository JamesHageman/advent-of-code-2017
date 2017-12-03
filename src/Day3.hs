module Day3 where

import qualified Data.Map.Strict as Map
import Control.Monad (foldM)
import Data.Monoid

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

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a, b) (c, d) = (a + c, b + d)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors pos =
  fmap (add pos) $ take 8 $ spiralFrom (1, 0)

sumMaybe :: [Maybe Int] -> Int
sumMaybe xs = getSum $ foldMap (maybe mempty Sum) xs

spiralWithNeighborSum :: Map.Map (Int, Int) Int -> (Int, Int) -> [(Int, (Int, Int))]
spiralWithNeighborSum state curr =
  (sum, curr) : spiralWithNeighborSum nextState (nextPosition curr)
  where
    sum = if curr == (0, 0) then 1 else
      sumMaybe $ fmap (\pos -> Map.lookup pos state) $ neighbors curr
    nextState = Map.insert curr sum state

day3part2 = fst $ head $ filter ((> 325489) . fst) $ spiralWithNeighborSum Map.empty (0, 0)
