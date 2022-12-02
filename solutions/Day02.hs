module Day02 where

import Solution (Solution (Single), Answer)

day02 :: Solution
day02 = Single part1

part1 :: String -> Answer
part1 input = show
    $ map (getScore . getStrategy)
    $ lines input

getScore :: (Shape, Shape) -> Int 

getStrategy :: String -> (Shape, Shape)
getStrategy (opponent : ' ' : you : _) = (charToShape opponent, charToShape you)
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
