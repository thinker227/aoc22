module Solution where

data Solution
    = Separate { part1 :: String -> Integer, part2 :: String -> Integer }
    | Combined { solve :: String -> (Integer, Integer) }

runSolution (Separate part1 part2) input = (part1 input, part2 input)
runSolution (Combined solve) input = solve input
