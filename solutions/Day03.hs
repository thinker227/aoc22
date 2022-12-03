module Day03 (day03) where

import Solution (Solution (Separate), Answer)
import ListUtils (splitHalf, chunk)
import Data.List (intersect, nub)
import Data.Char (ord)

day03 :: Solution
day03 = Separate part1 part2

part1 :: String -> Answer
part1 input = show
    $ sum
    $ map (priority . head . nub . uncurry intersect . splitHalf)
    $ lines input

part2 :: String -> Answer
part2 input = show
    $ sum
    $ map (priority . head . nub . \[a, b, c] -> intersect c $ intersect a b)
    $ chunk 3
    $ lines input

priority :: Char -> Int
priority c = let n = ord c in
    if c `elem` ['a'..'z']
    then n - ord 'a' + 1
    else (n - ord 'A') + 27
