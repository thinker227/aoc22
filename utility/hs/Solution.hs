module Solution where

type Answer = String

data Solution
    = Single { part1 :: String -> Answer }
    | Separate { part1 :: String -> Answer, part2 :: String -> Answer }
    | Combined { solve :: String -> (Answer, Answer) }

runSolution :: Solution -> String -> (Answer, Maybe Answer)
runSolution (Single part1) input = (part1 input, Nothing)
runSolution (Separate part1 part2) input = (part1 input, Just $ part2 input)
runSolution (Combined solve) input =
    let (p1, p2) = solve input in
    (p1, Just p2)
