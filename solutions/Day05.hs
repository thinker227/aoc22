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
    crates = init crates'
    in
        "\nCrate width: " ++ show width ++ "\nCrates: " ++ show crates

crateWidth :: String -> Int
crateWidth str = (length str + 1) `div` 4

parseCrateStack :: [String] -> (CrateStack, [String])
parseCrateStack ("":_) = ([], [])
parseCrateStack xs = let
    crates' = map (take 3) xs
    crates = filter (/= ' ') $ map (\[_,c,_] -> c) crates'
    in
        (crates, map (drop 4) xs)

type Crate = Char
type CrateStack = [Crate]
