module Day08 (day08) where

import Solution(Solution(Single), Answer)
import Data.Char (digitToInt)
import ListUtils (aheadTail)

day08 :: Solution
day08 = Single part1

part1 :: String -> Answer
part1 input = show
    $ map (
          map (\(ahead, x, tail) ->
            any (>= x) ahead &&
            any (>= x) tail)
        . aheadTail
        . map digitToInt)
    $ lines input
