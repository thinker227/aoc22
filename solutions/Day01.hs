module Day01 (day01) where

import Solution (Solution(Single), Answer)
import ListUtils (splitBy)
import StringUtils (blank)

day01 :: Solution
day01 = Single part1

part1 :: String -> Answer
part1 input = show
    $ maximum
    $ map (sum . (map read :: [String] -> [Int]))
    $ splitBy blank
    $ splitBy (== '\n') input
