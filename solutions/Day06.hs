module Day06 where
import Solution (Solution (Single), Answer)
import Data.List (nub)

day06 :: Solution
day06 = Single part1

part1 :: String -> Answer
part1 input = show
    $ markerPos
    $ zip input [0..length input]

markerPos xs =
    let es = take 4 $ map fst xs
    in if nub es == es
        then snd (head xs) + 4
        else markerPos $ tail xs
