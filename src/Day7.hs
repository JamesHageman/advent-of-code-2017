module Day7 where

import Text.Parsec

data Statement = Statement String Int [String] deriving (Show)

commaSep :: Parsec String m a -> Parsec String m [a]
commaSep p = p `sepBy1` (string "," *> spaces)

parens = between (string "(") (string ")")

word = many1 letter
int = read <$> many1 digit
arrow = space *> string "->" *> space
childNames = arrow *> commaSep word

statement = Statement <$> name <*> weight <*> children
  where
    name = word <* space
    weight = parens int
    children = try childNames <|> return []

input = statement `endBy1` endOfLine

day7part1 file = parse input "" <$> readFile file
