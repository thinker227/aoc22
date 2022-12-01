module Day01 (day01) where

import Solution (Solution(Separate), Answer)
import ListUtils (splitBy)
import StringUtils (blank)
import Data.List (sort)

day01 :: Solution
day01 = Separate part1 part2

part1 :: String -> Answer
part1 input = show
    $ maximum
    $ totalCalories input

part2 :: String -> Answer
part2 input = show
    $ sum
    $ take 3
    $ reverse
    $ sort
    $ totalCalories input

totalCalories input =
      map (sum . (map read :: [String] -> [Int]))
    $ splitBy blank
    $ splitBy (== '\n') input
