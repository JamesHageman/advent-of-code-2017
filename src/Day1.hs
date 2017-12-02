module Day1 where

readDigit :: Char -> Int
readDigit c = read $ [c]

digits :: String -> [Int]
digits = fmap readDigit

appendHead :: [a] -> [a]
appendHead list = list ++ [head list]

inAdjacentTuples :: [Int] -> [(Int, Int)]
inAdjacentTuples (x:y:rest) = (x,y) : inAdjacentTuples (y:rest)
inAdjacentTuples _ = []

inHalfwayTuples :: [Int] -> [(Int, Int)]
inHalfwayTuples list = fmap (\(x, i) ->
    (x, (list !! ((i + size `div` 2) `mod` size)))
  ) listWithIndices
  where
    listWithIndices = zip list [0..]
    size = length list

sumMatchingDigits :: [(Int, Int)] -> Int
sumMatchingDigits ((x,y):rest) =
  if x == y then
    x + sumMatchingDigits rest
    else sumMatchingDigits rest
sumMatchingDigits [] = 0

day1part1 :: String -> Int
day1part1 = sumMatchingDigits . inAdjacentTuples . appendHead . digits

day1part2 :: String -> Int
day1part2 = sumMatchingDigits . inHalfwayTuples . digits
