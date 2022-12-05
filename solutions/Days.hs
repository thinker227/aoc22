module Days where

import Data.Map (Map, fromList)
import Solution (Solution)
import Day01
import Day02
import Day03
import Day04
import Day05

days :: Map Int Solution
days = fromList [
    (1, day01),
    (2, day02),
    (3, day03),
    (4, day04),
    (5, day05)
    ]