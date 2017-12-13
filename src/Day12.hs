module Day12 where

import Text.Parsec
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import Data.List (unfoldr)

int :: Parsec String m Int
int = read <$> many1 digit

input :: Parsec String m [(Int, [Int])]
input = many1 $ do
  x <- int
  string " <-> "
  xs <- int `sepBy` string ", "
  endOfLine
  pure (x, xs)

group :: Int -> M.IntMap [Int] -> S.IntSet
group x neighbors = go S.empty x
  where
    go visited x =
      if S.member x visited then
        visited
        else
          foldl go (S.insert x visited) $ M.findWithDefault [] x neighbors

groups :: M.IntMap [Int] -> [S.IntSet]
groups neighbors = unfoldr go neighbors
  where
    go ns
      | M.null ns = Nothing
      | otherwise = let g = group (head $ M.keys ns) ns in
        Just (g, ns `M.difference` M.fromSet id g)

day12 s = S.size . group 0 . M.fromList <$> parse input "" s
day12' s = length . groups . M.fromList <$> parse input "" s
