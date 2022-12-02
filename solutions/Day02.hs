module Day02 where

import Solution (Solution (Single), Answer)

day02 :: Solution
day02 = Single part1

part1 :: String -> Answer
part1 input = show
    $ sum
    $ map (getScore . getStrategy)
    $ lines input

getScore :: (Shape, Shape) -> Int
getScore (Rock, s) = 1 + case s of
    Rock -> 3
    Paper -> 0
    Scissors -> 6
getScore (Paper, s) = 2 + case s of
   Rock -> 6
   Paper -> 3
   Scissors -> 0
getScore (Scissors, s) = 3 + case s of
   Rock -> 0
   Paper -> 6
   Scissors -> 3

getStrategy :: String -> (Shape, Shape)
getStrategy (b : ' ' : a : _) = (charToShape a, charToShape b)
getStrategy _ = error "Malformed input"

charToShape :: Char -> Shape
charToShape 'A' = Rock
charToShape 'B' = Paper
charToShape 'C' = Scissors
charToShape 'X' = Rock
charToShape 'Y' = Paper
charToShape 'Z' = Scissors
charToShape _ = error "Malformed input"

data Shape
    = Rock
    | Paper
    | Scissors
    deriving (Eq, Show)
