module Day6 where

import qualified Data.Set as Set
import Data.Vector as Vector
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST

type Bank = Int
type Memory = Vector Bank

parseMemory :: String -> Memory
parseMemory = fromList . fmap read . words

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

numRebalancesUntilSeenConfiguration :: Memory -> Int
numRebalancesUntilSeenConfiguration memory = go Set.empty memory 1
  where
    go seenMemories memory numIterations =
      let newMemory = rebalance memory in
      if Set.member newMemory seenMemories then
        numIterations
        else go (Set.insert newMemory seenMemories) newMemory (numIterations + 1)

day6part1 = numRebalancesUntilSeenConfiguration . parseMemory <$> readFile "test/Day6.input"
