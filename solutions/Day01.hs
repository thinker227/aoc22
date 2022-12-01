module Day01 (day01) where

import Solution (Solution(Separate), Answer)
import ListUtils (splitBy)
import StringUtils (blank)
import NumUtils (readNum)
import Data.List (sort)

day01 :: Solution
day01 = Separate part1 part2

part1 :: String -> Answer
part1 input = show
    $ head
    $ totalCalories input

part2 :: String -> Answer
part2 input = show
    $ sum
    $ take 3
    $ totalCalories input

totalCalories :: String -> [Int]
totalCalories input =
      reverse
    $ sort
    $ map (sum . map readNum)
    $ splitBy blank
    $ splitBy (== '\n') input
