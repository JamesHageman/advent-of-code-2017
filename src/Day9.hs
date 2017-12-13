{-# LANGUAGE TemplateHaskell #-}
module Day9 where

import Test.HUnit
import Control.Lens

data ParseState = ParseState { _depth :: Int, _inGarbage :: Bool, _score :: Int, _garbageCount  :: Int }
makeLenses ''ParseState


compute :: String -> ParseState
compute = go (ParseState 0 False 0 0)
  where
    go state ('!':_:rest) = go state rest
    go state@ParseState { _inGarbage = False } ('<':rest) = go (set inGarbage True state) rest
    go state ('>':rest) = go (set inGarbage False state) rest
    go state@ParseState { _inGarbage = False } ('{':rest) =
      go (over depth succ state) rest
    go state@ParseState { _inGarbage = False } ('}':rest) =
      go (over depth pred $ over score (+ state^.depth) state) rest
    go state@ParseState { _inGarbage = True } (_:rest) = go (over garbageCount succ state) rest
    go state (_:rest) = go state rest
    go state [] = state

day9 :: String -> Int
day9 = _score . compute

day9' :: String -> Int
day9' = _garbageCount . compute

part1Tests :: Test
part1Tests = TestCase $ do
  assertEqual "1" 5 $ day9 "{{},{}}"
  assertEqual "2" 1 $ day9 "{}"
  assertEqual "3" 6 $ day9 "{{{}}}"
  assertEqual "4" 16 $ day9 "{{{},{},{{}}}}"
  assertEqual "5" 9 $ day9 "{{<ab>},{<ab>},{<ab>},{<ab>}}"
  assertEqual "6" 9 $ day9 "{{<!!>},{<!!>},{<!!>},{<!!>}}"
  assertEqual "7" 3 $ day9 "{{<a!>},{<a!>},{<a!>},{<ab>}}"

part2Tests :: Test
part2Tests = TestCase $ do
  assertEqual "1" 0 $ day9' "<!!!>>"
  assertEqual "2" 17 $ day9' "<random characters>"
  assertEqual "3" 10 $ day9' "<{o\"i!a,<{i<a>"

tests :: IO Counts
tests = runTestTT $ TestList 
  [ TestLabel "Part 1" part1Tests
  , TestLabel "Part 2" part2Tests
  ]

-- do "run day9" or "run day9'" in ghci
run :: (String -> a) -> IO a
run f = f <$> readFile "test/Day9.input"
