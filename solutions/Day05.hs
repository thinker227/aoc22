module Day05 where
import Solution (Solution (Single), Answer)
import ListUtils (splitBy)
import Data.List (elemIndex)
import StringUtils (blank)

day05 :: Solution
day05 = Single part1

part1 :: String -> Answer
part1 input = let
    ls = lines input
    [crates', instructions] = splitBy blank ls
    width = crateWidth $ head crates'
    stacks = parseCrates $ init crates'
    in "\nStacks: " ++ show stacks

crateWidth :: String -> Int
crateWidth str = (length str + 1) `div` 4

parseCrates [] = []
parseCrates xs = let
    (stack, rest) = parseCrateStack xs
    in if stack /= []
        then stack : parseCrates rest
        else []

parseCrateStack :: [String] -> (CrateStack, [String])
parseCrateStack ("":_) = ([], [])
parseCrateStack xs = let
    crates = filter (/= ' ')
        $ map ((\[_,c,_] -> c) . take 3) xs
    in (crates, map (drop 4) xs)

type Crate = Char
type CrateStack = [Crate]
