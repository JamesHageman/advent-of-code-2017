module Day1 where

readDigit :: Char -> Integer
readDigit c = read $ [c]

digits :: String -> [Integer]
digits = fmap readDigit

appendHead :: [a] -> [a]
appendHead list = list ++ [head list]

inAdjacentTuples :: [Integer] -> [(Integer, Integer)]
inAdjacentTuples (x:y:rest) = (x,y) : inAdjacentTuples (y:rest)
inAdjacentTuples _ = []

inHalfwayTuples :: [Integer] -> [(Integer, Integer)]
inHalfwayTuples list = fmap (\(x, i) ->
    (x, (list !! (i + size `div` 2) `mod` size))
  ) listWithIndices
  where
    listWithIndices = zip list [0..]
    size = length list

sumMatchingDigits :: [(Integer, Integer)] -> Integer
sumMatchingDigits ((x,y):rest) =
  if x == y then
    x + sumMatchingDigits rest
    else sumMatchingDigits rest
sumMatchingDigits [] = 0

day1part1 :: String -> Integer
day1part1 = sumMatchingDigits . inAdjacentTuples . appendHead . digits

day1part2 :: String -> Integer
day1part2 = sumMatchingDigits . inHalfwayTuples . digits
