module Day7 where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

data Statement = Statement String Int [String] deriving (Show)

lexer = P.makeTokenParser emptyDef

int = fromIntegral <$> P.integer lexer

programName = many1 letter

childNames = do
  string " -> "
  programName `sepBy1` string ", "

weight = do
  string "("
  w <- int
  string ")"
  return w

statement = do
  name <- programName
  space
  weight <- weight
  children <- option [] childNames
  return $ Statement name weight children

input = statement `endBy1` endOfLine

day7part1 file = parse input "" <$> readFile file
