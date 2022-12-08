module Day08 (day08) where

import Solution(Solution(Separate), Answer)
import Data.Char (digitToInt)
import ListUtils (aheadTail, columns)

day08 :: Solution
day08 = Separate part1 part2

part1 :: String -> Answer
part1 input = show
    $ length
    $ filter (== False)
    $ concat
    $ gridApply hiddenLine (&&)
    $ lines input

part2 :: String -> Answer
part2 input = show
    $ maximum
    $ concat
    $ gridApply scenicLine (*)
    $ map (map digitToInt)
    $ lines input

gridApply op zip xs = let
    hor = map op xs
    ver = columns $ map op $ columns xs
    in zipWith (zipWith zip) hor ver

hiddenLine xs =
    map (\(ahead, x, tail) ->
        any (>= x) ahead &&
        any (>= x) tail)
    $ aheadTail xs

scenicLine xs =
    map (\(ahead, x, tail) ->
        scenic x (reverse ahead) * scenic x tail)
    $ aheadTail xs

scenic x xs =
    min (length xs) $ length (takeWhile (< x) xs) + 1
