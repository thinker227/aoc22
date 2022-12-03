module Day03 (day03) where

import Solution (Solution (Single), Answer)
import ListUtils (splitHalf)
import Data.List (intersect, nub)
import Data.Char (ord)

day03 :: Solution
day03 = Single part1

part1 :: String -> Answer
part1 input = show
    $ sum
    $ map (priority . head . nub . uncurry intersect . splitHalf)
    $ lines input

priority :: Char -> Int
priority c = let n = ord c in
    if c `elem` ['a'..'z']
    then n - ord 'a' + 1
    else (n - ord 'A') + 27
