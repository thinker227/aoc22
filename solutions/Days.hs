module Days where

import Data.Map (Map, fromList)
import Solution (Solution)
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day13
import Day14

days :: Map Int Solution
days = fromList [
    (1, day01),
    (2, day02),
    (3, day03),
    (4, day04),
    (5, day05),
    (6, day06),
    (7, day07),
    (8, day08),
    (9, day09),
    (10, day10),
    (11, day11),
    (13, day13),
    (14, day14)
    ]
