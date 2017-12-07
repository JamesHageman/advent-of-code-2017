module Lib
    ( currentMain
    ) where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6

currentMain = day5part2 "test/Day5.input" >>= print
