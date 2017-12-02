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
