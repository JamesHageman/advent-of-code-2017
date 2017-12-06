{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module Day5 where

import qualified Data.IntMap.Strict as M
import Control.Monad.State.Strict
import Control.Lens
import Control.Lens.Operators
import Debug.Trace

type Jumps = M.IntMap Int
data JumpState = JumpState { _jumps :: Jumps, _position :: Int, _numJumps :: Int } deriving (Show)
makeLenses ''JumpState

parseJumps :: String -> Jumps
parseJumps = M.fromList . zip [0..] . fmap read . words

executeJumps :: State JumpState Int
executeJumps = do
  state <- get

  case state^.jumps.at (state^.position) of
    Nothing -> return $ state^.numJumps
    Just offset -> do
      put $ JumpState
        (M.adjust (+1) (state^.position) (state^.jumps))
        (state^.position + offset)
        (state^.numJumps + 1)
      executeJumps

executeJumps2 :: JumpState -> Int
executeJumps2 state =
  case M.lookup (_position (trace (show state) state)) (_jumps state) of
    Nothing -> _numJumps state
    Just offset -> do
       executeJumps2 $ JumpState
        (M.insert (_position state) (if offset >= 3 then offset - 1 else offset + 1) (_jumps state))
        (_position state + offset)
        (_numJumps state + 1)

initialState :: Jumps -> JumpState
initialState jumps = JumpState jumps 0 0

day5part1 file = evalState executeJumps . initialState . parseJumps <$> readFile file
day5part2 file = executeJumps2 . initialState . parseJumps <$> readFile file
