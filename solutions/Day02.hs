module Day02 where

import Solution (Solution (Separate), Answer)
import GHC.Stack (HasCallStack)

day02 :: Solution
day02 = Separate (solve p1Strat) (solve p2Strat)

solve :: (String -> (Shape, Shape)) -> String -> Answer
solve strategy input = show
    $ sum
    $ map (getScore . strategy)
    $ lines input

-- | Gets the score of a game (player shape, opponent shape).
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

-- | Parse into a tuple (player shape, opponent shape).
p1Strat :: String -> (Shape, Shape)
p1Strat (b : ' ' : a : _) = (charToShape a, charToShape b)
p1Strat _ = error "Malformed input"

-- | Parse into a tuple (shape to win\/draw\/loose against opponent, opponent shape).
p2Strat :: String -> (Shape, Shape)
p2Strat (b : ' ' : a : _) = (whatToPick a opponent, opponent)
    where opponent = charToShape b
p2Strat _ = error "Malformed input"

-- | Gets what shape to pick based on a character and the opponent's shape.
whatToPick :: Char -> Shape -> Shape
whatToPick 'X' opponent = case opponent of
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper
whatToPick 'Y' opponent = opponent
whatToPick 'Z' opponent = case opponent of
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock
whatToPick _ _ = error "Malformed input"

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
