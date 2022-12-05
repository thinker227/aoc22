module Day05 where
import Solution (Solution (Single), Answer)
import StringUtils (blank)
import NumUtils (readNumToEnd)
import ListUtils (splitBy, setAt)
import Data.List (elemIndex)

day05 :: Solution
day05 = Single part1

type Crate = Char
type CrateStack = [Crate]
data Instruction = Instruction {
    amount :: Int,
    from :: Int,
    to :: Int }
    deriving (Eq, Show)

part1 :: String -> Answer
part1 input = let
    ls = lines input
    [crates', instructions'] = splitBy blank ls
    stacks = parseCrates $ init crates'
    width = length stacks
    instructions = map parseInstruction instructions'
    in show
        $ map head
        $ rearrange stacks instructions

parseCrates :: [String] -> [CrateStack]
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

-- I could use State for this but I'm too lazy
parseInstruction :: String -> Instruction
parseInstruction str = let
    -- "move (amount)"
    (amount, str') = readNumToEnd $ drop 5 str
    -- " from (from)"
    (from, str'') = readNumToEnd $ drop 6 str'
    -- " to (to)"
    (to, _) = readNumToEnd $ drop 4 str''
    in Instruction amount from to

-- Could also use State here
rearrange :: [CrateStack] -> [Instruction] -> [CrateStack]
rearrange = foldl applyInstruction

applyInstruction :: [CrateStack] -> Instruction -> [CrateStack]
applyInstruction stacks (Instruction amount from to) = let
    -- The from and to values are 1-indexed
    fromStack = stacks !! (from - 1)
    toStack = stacks !! (to - 1)
    selection = take amount fromStack
    newFromStack = drop amount fromStack
    newToStack = reverse selection ++ toStack
    in setAt (from - 1) newFromStack $ setAt (to - 1) newToStack stacks
