module Day08 (day08) where

import Solution(Solution(Single), Answer)
import Data.Char (digitToInt)
import ListUtils (aheadTail, columns)

day08 :: Solution
day08 = Single part1

part1 :: String -> Answer
part1 input = show
    $ length
    $ filter (== False)
    $ concat
    $ gridHidden
    $ lines input

gridHidden xs = let
    hor = map hidden xs
    ver = columns $ map hidden $ columns xs
    in zipWith (zipWith (&&)) hor ver

hidden xs =
    map (\(ahead, x, tail) ->
        any (>= x) ahead &&
        any (>= x) tail)
    $ aheadTail xs
