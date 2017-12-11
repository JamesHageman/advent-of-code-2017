{-# LANGUAGE TemplateHaskell #-}
module Day9 where

import Test.HUnit
import Control.Lens

data ParseState = ParseState { _depth :: Int, _inGarbage :: Bool, _score :: Int }
makeLenses ''ParseState


day9 :: String -> Int
day9 input = _score $ go (ParseState 0 False 0) input
  where
    go state ('!':_:rest) = go state rest
    go state ('<':rest) = go (set inGarbage True state) rest
    go state ('>':rest) = go (set inGarbage False state) rest
    go state@ParseState { _inGarbage = False } ('{':rest) =
      go (over depth succ state) rest
    go state@ParseState { _inGarbage = False } ('}':rest) =
      go (over depth pred $ over score (+ state^.depth) state) rest
    go state (_:rest) = go state rest
    go state [] = state

part1Tests = TestCase $ do
  assertEqual "1" 5 $ day9 "{{},{}}"
  assertEqual "2" 1 $ day9 "{}"
  assertEqual "3" 6 $ day9 "{{{}}}"
  assertEqual "4" 16 $ day9 "{{{},{},{{}}}}"
  assertEqual "5" 9 $ day9 "{{<ab>},{<ab>},{<ab>},{<ab>}}"
  assertEqual "6" 9 $ day9 "{{<!!>},{<!!>},{<!!>},{<!!>}}"
  assertEqual "7" 3 $ day9 "{{<a!>},{<a!>},{<a!>},{<ab>}}"

tests = runTestTT $ TestList 
  [ TestLabel "Part 1" part1Tests
  ]

run = day9 <$> readFile "test/Day9.input"
