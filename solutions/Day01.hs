module Day01 (day01) where

import Solution (Solution(Combined), Answer)
import ListUtils (splitBy)
import StringUtils (blank)
import NumUtils (readNum)
import Data.List (sort)

day01 :: Solution
day01 = Combined solve

solve :: String -> (Answer, Answer)
solve input =
    let cals = totalCalories input in (
        show $ head cals,         -- Part 1
        show $ sum $ take 3 cals) -- Part 2

totalCalories :: String -> [Int]
totalCalories input =
      reverse
    $ sort
    $ map (sum . map readNum)
    $ splitBy blank
    $ lines input
