module Day06 where
import Solution (Solution (Single), Answer)
import Data.List (nub)

day06 :: Solution
day06 = Single part1

part1 :: String -> Answer
part1 input = show
    $ markerPos
    $ zip input [0..length input]

markerPos :: [(Char, Int)] -> (Char, Int)
markerPos xs =
    if nub (map fst $ take 4 xs) == map fst xs
    then head xs
    else markerPos $ tail xs
