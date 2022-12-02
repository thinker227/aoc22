module Day02 where

import Solution (Solution (Single), Answer)

day02 :: Solution
day02 = Single part1

part1 :: String -> Answer
part1 input = show
    $ map (getScore . getStrategy)
    $ lines input

getScore :: (Move, Move) -> Int

getStrategy :: String -> (Move, Move)
getStrategy (opponent : ' ' : you : _) = (charToMove opponent, charToMove you)
getStrategy _ = error "Malformed input"

charToMove :: Char -> Move
charToMove 'A' = Rock
charToMove 'B' = Paper
charToMove 'C' = Scissors
charToMove 'X' = Rock
charToMove 'Y' = Paper
charToMove 'Z' = Scissors
charToMove _ = error "Malformed input"

data Move
    = Rock
    | Paper
    | Scissors
    deriving (Eq, Show)
