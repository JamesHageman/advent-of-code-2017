{-# LANGUAGE TemplateHaskell #-}

module Day5 where

import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Lens
import Control.Lens.Operators
import Debug.Trace

type Jumps = M.Map Int Int
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

initialState :: Jumps -> JumpState
initialState jumps = JumpState jumps 0 0

day5part1 file = evalState executeJumps . initialState . parseJumps <$> readFile file
