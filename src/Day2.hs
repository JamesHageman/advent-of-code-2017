module Day2 where

type Spreadsheet = [[Int]]

parseRow :: String -> [Int]
parseRow = fmap read . words

parseSpreadsheet :: String -> Spreadsheet
parseSpreadsheet = fmap parseRow . lines

rowRange :: [Int] -> Int
rowRange row = maximum row - minimum row

checksum :: Spreadsheet -> Int
checksum = sum . fmap rowRange

day2part1 :: FilePath -> IO Int
day2part1 = fmap (checksum . parseSpreadsheet) . readFile

a `divides` b = b `mod` a == 0

firstDivisor :: [Int] -> Int -> Maybe Int
firstDivisor (x:xs) y =
  if x `divides` y then
    Just x
    else firstDivisor xs y
firstDivisor [] y = Nothing

findDivisiblePair :: [Int] -> Maybe (Int, Int)
findDivisiblePair fullList = search fullList fullList
  where
    search fullList (x:xs) =
      case firstDivisor (filter (/= x) fullList) x of
        Just y -> Just (x, y)
        Nothing -> search fullList xs
    search fullList [] = Nothing

rowQuotient :: [Int] -> Int
rowQuotient row =
  case findDivisiblePair row of
    Just (hi, lo) -> hi `div` lo
    Nothing -> 0

day2part2 :: FilePath -> IO Int
day2part2 = fmap (sum . fmap rowQuotient . parseSpreadsheet) . readFile
