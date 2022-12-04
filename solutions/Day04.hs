module Day04 where

import Solution (Solution (Single), Answer)
import NumUtils (readNum)
import Data.List (sortBy, intersect)
import ListUtils (splitBy)

day04 :: Solution
day04 = Single part1

part1 :: String -> Answer
part1 input = show
    $ length
    $ filter (\[xs, ys] -> all (`elem` ys) xs)
    $ map (
          sortBy (\xs ys -> compare (length xs) (length ys))
        . map toRange
        . splitBy (== ','))
    $ lines input

toRange :: String -> [Int]
toRange str = let [a, b] = splitBy (== '-') str in
    [readNum a..readNum b]
