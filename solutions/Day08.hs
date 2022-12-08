module Day08 (day08, scenic, scenicLine, scenicGrid) where

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
    $ gridHidden
    $ lines input

part2 :: String -> Answer
part2 input = show
    $ maximum
    $ concat
    $ scenicGrid
    $ map (map digitToInt)
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

scenicGrid xs = let
    hor = map scenicLine xs
    ver = columns $ map scenicLine $ columns xs
    in zipWith (zipWith (*)) hor ver

scenicLine xs =
    map (\(ahead, x, tail) ->
        scenic x (reverse ahead) * scenic x tail)
    $ aheadTail xs

scenic x xs =
    min (length xs) $ length (takeWhile (< x) xs) + 1
