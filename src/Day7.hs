module Day7 where

import Text.Parsec
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict as S
import Data.List (find)

type Pid = String
data Statement = Statement Pid Int [Pid] deriving (Show)
data Process = Process Pid Int deriving (Show)
data SupervisionTree
  = Node Process (M.Map String SupervisionTree)
  | Leaf Process
  deriving (Show)

-- Parsing

commaSep :: Parsec String m a -> Parsec String m [a]
commaSep p = p `sepBy1` (string "," *> spaces)

parens = between (string "(") (string ")")

pid = many1 letter
int = read <$> many1 digit
arrow = space *> string "->" *> space
childNames = arrow *> commaSep pid

statement = Statement <$> name <*> weight <*> children
  where
    name = pid <* space
    weight = parens int
    children = try childNames <|> return []

input = statement `endBy1` endOfLine

-- Computation

removeKeys :: Ord k => M.Map k a -> [k] -> M.Map k a
removeKeys map keys = foldl (\m deleteF -> deleteF m) map fs
  where fs = M.delete <$> keys

statementPid (Statement pid _ _) = pid

treePid (Node (Process pid _) _) = pid
treePid (Leaf (Process pid _)) = pid

buildNode :: Process -> [SupervisionTree] -> SupervisionTree
buildNode process trees = Node process $ M.fromList $ withPid <$> trees
  where
    withPid tree = (treePid tree, tree)

insertTree :: SupervisionTree -> S.State ([Statement], M.Map Pid SupervisionTree) SupervisionTree
insertTree tree = do
  let pid = treePid tree
  (statements, trees) <- get
  let
    newStatements = filter ((/= pid) . statementPid) statements
    filteredTrees = case tree of
      Node _ children -> removeKeys trees (fst <$> M.toList children)
      Leaf _ -> trees 
    newTrees = M.insert pid tree filteredTrees
  put (newStatements, newTrees)
  return tree

resolveStatement :: Pid -> S.State ([Statement], M.Map Pid SupervisionTree) SupervisionTree
resolveStatement pid = do
  (statements, _) <- get
  case find ((== pid) . statementPid) statements of
    Just (Statement pid weight []) -> insertTree $ Leaf (Process pid weight)
    Just (Statement pid weight children) -> do
      trees <- sequence $ resolveTree <$> children
      let process = Process pid weight
      let tree = buildNode process trees
      insertTree tree
    Nothing -> undefined
 
resolveTree :: Pid -> S.State ([Statement], M.Map Pid SupervisionTree) SupervisionTree
resolveTree pid = do
  (statements, resolvedTrees) <- get
  case M.lookup pid resolvedTrees of
    Just tree -> return tree
    Nothing -> resolveStatement pid

computeRoot :: S.State ([Statement], M.Map Pid SupervisionTree) SupervisionTree
computeRoot = do
  (statements, trees) <- get
  case (statements, trees) of
    (s:_, _) -> do
      resolveTree $ statementPid s
      computeRoot
    ([], trees) -> return $ snd $ head $ M.toList trees

initialState statements = (statements, M.empty)
toTree :: [Statement] -> SupervisionTree
toTree statements = S.evalState computeRoot (statements, M.empty)

weight :: SupervisionTree -> Int
weight (Leaf (Process _ weight)) = weight
weight (Node (Process _ w) children) = w + childWeight
  where childWeight = sum $ weight . snd <$> M.toList children

day7part1 file = do 
  x <- readFile file
  return $ (treePid . toTree) <$> parse input "" x
