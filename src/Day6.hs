module Day6 where

import Data.Map.Strict as Map
import Data.Vector as Vector
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST

type Bank = Int
type Memory = Vector Bank

parseMemory :: String -> Memory
parseMemory = Vector.fromList . fmap read . words

largestBankWithIndex :: Memory -> (Int, Bank)
largestBankWithIndex memory = Vector.ifoldl findMax (0, Vector.head memory) memory
  where 
    findMax max@(j, maxbank) i curr =
      if curr > maxbank then
        (i, curr)
        else max

rebalance :: Memory -> Memory
rebalance memory = runST $ do
  let (initialIndex, initialValue) = largestBankWithIndex memory
  v <- thaw memory
  MV.write v initialIndex 0
  redistribute (initialIndex + 1) initialValue v
  freeze v

redistribute _ 0 _ = return ()
redistribute i remainingValue v = do
  let j = i `mod` (MV.length v)
  MV.modify v (+1) j
  redistribute (j + 1) (remainingValue - 1) v

runIteration :: Map Memory Int -> Memory -> Int -> (Int, Int)
runIteration seenMemories memory numIterations =
  let newMemory = rebalance memory in
  case Map.lookup newMemory seenMemories of
    Just index -> (numIterations, numIterations - index)
    Nothing -> runIteration
      (Map.insert newMemory numIterations seenMemories)
      newMemory
      (numIterations + 1)

runIterations :: Memory -> (Int, Int)
runIterations memory = runIteration Map.empty memory 1

numRebalancesUntilSeenConfiguration :: Memory -> Int
numRebalancesUntilSeenConfiguration = fst . runIterations

numCycles :: Memory -> Int
numCycles = snd . runIterations

runOnInput f = f . parseMemory <$> readFile "test/Day6.input"

day6part1 = runOnInput numRebalancesUntilSeenConfiguration
day6part2 = runOnInput numCycles
