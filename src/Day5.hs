module Day5 where

import qualified Data.IntMap.Strict as M

type Jumps = M.IntMap Int

parseJumps :: String -> Jumps
parseJumps = M.fromList . zip [0..] . fmap read . words

numJumpsToEscape :: (Int -> Int) -> Jumps -> Int
numJumpsToEscape f jumps = go f jumps 0 0
  where
    go f jumps pos numJumps =
      case M.lookup pos jumps of
        Nothing -> numJumps
        Just offset ->
          go f (M.insert pos (f offset) jumps) (pos + offset) (numJumps + 1)

runFile :: (Int -> Int) -> FilePath -> IO Int
runFile f file = numJumpsToEscape f . parseJumps <$> readFile file

day5part1 = runFile (+1)
day5part2 = runFile (\x -> if x >= 3 then x - 1 else x + 1)
