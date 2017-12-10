module Day8 where

import Text.Parsec
import Data.Functor
import Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import qualified Data.Map.Strict as M
import Data.Maybe

data Instr = Instr Reg Op Int Reg Cond Int deriving (Show)
type Reg = String
data Op = Inc | Dec deriving (Show)
data Cond = Eq | Ne | Lt | Gt | Le | Ge deriving (Show)
type Registers = M.Map Reg Int

lexer = P.makeTokenParser emptyDef

op :: Parsec String m Op
op = (string "inc" $> Inc) <|> try (string "dec" $> Dec)

cond :: Parsec String m Cond
cond =
  try (string "<=" $> Le) <|>
  try (string "<" $> Lt) <|> 
  try (string ">=" $> Ge) <|>
  try (string ">" $> Gt) <|>
  try (string "!=" $> Ne) <|>
  try (string "==" $> Eq)

int :: Parsec String m Int
int = fromIntegral <$> P.integer lexer

reg :: Parsec String m String
reg = many1 letter

instr :: Parsec String m Instr
instr =
  Instr <$>
  (reg <* spaces) <*>
  (op <* spaces) <*>
  (int <* spaces) <*>
  (string "if" *> spaces *> reg <* spaces) <*>
  (cond <* spaces) <*>
  int


condition :: Cond -> Int -> Int -> Bool
condition Ne = (/=)
condition Eq = (==)
condition Lt = (<)
condition Gt = (>)
condition Le = (<=)
condition Ge = (>=)

operation :: Op -> Int -> Int -> Int
operation Inc = (+)
operation Dec = (-)

instructions = many1 instr

registerValue :: Reg -> Registers -> Int
registerValue = M.findWithDefault 0

applyInstruction :: Registers -> Instr -> Registers
applyInstruction registers (Instr r1 op x r2 cond y) =
  if registerValue r2 registers `pred` y then
    M.alter (Just . f x . fromMaybe 0) r1 registers 
    else registers
  where
    f = operation op
    pred = condition cond

compute :: [Instr] -> Registers
compute = foldl applyInstruction M.empty

maxValue :: Registers -> Int
maxValue registers =
  case snd <$> M.toList registers of
    [] -> 0
    values -> maximum values

parseFile f file = (fmap f . parse instructions "") <$> readFile file

day8part1 = parseFile $ maxValue . compute
