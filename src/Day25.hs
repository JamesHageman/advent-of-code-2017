{-# LANGUAGE TemplateHaskell #-}
module Day25 where

import Control.Lens
import qualified Data.IntSet as S
import qualified Data.Map.Strict as M
import Text.Parsec
import Prelude hiding (Left, Right)
import Data.Functor (($>))

data Direction = Left | Right deriving (Show)

data Machine = Machine {
  _tape :: S.IntSet,
  _cursor :: Int,
  _state :: Char,
  _checksumIn :: Int,
  _transitions :: M.Map (Char, Bool) (Char, Bool, Direction)
} deriving (Show)
makeLenses ''Machine

parseMachine :: Parsec String m Machine
parseMachine = Machine emptyTape 0 <$> initialState <*> checksumAt <*> parseTransitions

emptyTape = S.empty

initialState = do
  string "Begin in state "
  s <- parseState
  string "."
  whitespace
  pure s

checksumAt :: Parsec String m Int
checksumAt = do
  string "Perform a diagnostic checksum after "
  x <- fromIntegral . read <$> many1 digit
  string " steps."
  pure x

parseTransitions :: Parsec String m (M.Map (Char, Bool) (Char, Bool, Direction))
parseTransitions = fmap M.unions $ many1 $ do
  whitespace
  string "In state "
  s <- parseState
  string ":"
  whitespace
  t1 <- parseTransition s
  t2 <- parseTransition s
  return $ M.fromList [t1, t2]

parseTransition :: Char -> Parsec String m ((Char, Bool), (Char, Bool, Direction))
parseTransition startingState = do
  string "If the current value is "
  isOne <- (== '1') <$> digit
  string ":"
  whitespace
  string "- Write the value "
  writeOne <- (== '1') <$> digit
  string "."
  whitespace
  string "- Move one slot to the "
  direction <- parseDirection
  string "."
  whitespace
  string "- Continue with state "
  nextState <- parseState
  string "."
  whitespace
  pure ((startingState, isOne), (nextState, writeOne, direction))
  
parseDirection = (string "left" $> Left) <|> (string "right" $> Right)

parseState = letter
whitespace = many (space <|> endOfLine)

runMachine :: Machine -> Int
runMachine Machine {_checksumIn = 0, _tape = tape} = S.size tape 
runMachine m =
  case M.lookup (currentState, isOne) (m ^. transitions) of
    Nothing -> undefined
    Just (nextState, writeOne, direction) ->
      let
        updateMachine =
          set state nextState 
          . over cursor (applyDirection direction)
          . over tape (if writeOne then S.insert currentCursor else S.delete currentCursor)
          . over checksumIn pred
      in runMachine $ updateMachine m
  where
    currentState = m ^. state
    currentCursor = m ^. cursor
    isOne = S.member currentCursor (m ^. tape)

applyDirection Left = pred
applyDirection Right = succ

day s = runMachine <$> parse parseMachine "" s
