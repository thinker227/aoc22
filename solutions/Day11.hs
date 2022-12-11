module Day11 where

import Solution(Solution(Single), Answer)
import ListUtils (splitBy, setAt, modifyAt)
import StringUtils (blank, trim)
import NumUtils (readNum)
import Data.Char (digitToInt)

day11 :: Solution
day11 = Single part1

data Monkey = Monkey {
    monkeyNumber :: Int,
    items :: [Int],
    op :: Operation,
    test :: Int,
    ifTrue :: Int,
    ifFalse :: Int }
    deriving (Show)

data Operation = Operation {
    operator :: Operator,
    value :: Value }
    deriving (Show)

data Operator
    = Mult
    | Plus
    deriving (Show)

data Value
    = Number { val :: Int }
    | Id
    deriving (Show)

part1 :: String -> Answer
part1 input = let
    ms = map parseMonkey
        $ splitBy blank
        $ lines input
    in show
    $ inspectAll ms

parseMonkey :: [String] -> Monkey
parseMonkey (l0:l1:l2:l3:l4:l5:_) = let
    monkeyNumber = digitToInt $ l0 !! 7
    items = map (readNum . trim)
        $ splitBy (== ',')
        $ drop 18 l1
    operator = if (l2 !! 23) == '*'
        then Mult
        else Plus
    value = let v = drop 25 l2 in
        if v == "old"
            then Id
            else Number (readNum v)
    test = readNum $ drop 21 l3
    ifTrue = readNum $ drop 29 l4
    ifFalse = readNum $ drop 30 l5
    in Monkey monkeyNumber items (Operation operator value) test ifTrue ifFalse
parseMonkey _ = error "Malformed input"

updateItems :: ([Int] -> [Int]) -> Monkey -> Monkey
updateItems f (Monkey monkeyNumber items op test ifTrue ifFalse) =
    Monkey monkeyNumber (f items) op test ifTrue ifFalse

addItems :: [Int] -> Monkey -> Monkey
addItems xs = updateItems (++ xs)

removeHeadItem :: Monkey -> Monkey
removeHeadItem = updateItems tail

inspectAll :: [Monkey] -> [Monkey]
inspectAll = inspectAll' 0

inspectAll' :: Int -> [Monkey] -> [Monkey]
inspectAll' i ms = if i >= length ms
    then ms
    else let
        m = ms !! i
        (_, ms') = inspect m ms
        in inspectAll' (i + 1) ms'

inspect :: Monkey -> [Monkey] -> (Monkey, [Monkey])
inspect m ms = let
    (m', true, false) = inspectItems m
    ms' = modifyAt (ifFalse m) (addItems false)
        $ modifyAt (ifTrue m) (addItems true)
        $ setAt (monkeyNumber m) m' ms
    in (m', ms')

inspectItems :: Monkey -> (Monkey, [Int], [Int])
inspectItems m = inspectItems' m [] []

inspectItems' :: Monkey -> [Int] -> [Int] -> (Monkey, [Int], [Int])
inspectItems' m true false = if null (items m)
    then (m, true, false)
    else let (m', true', false') = inspectHeadItem m
        in inspectItems' m' (true ++ true') (false ++ false')

inspectHeadItem :: Monkey -> (Monkey, [Int], [Int])
inspectHeadItem m = let
    v = head (items m)
    operation = op
    worryLevel = applyOperation (operation m) v `div` 3
    success = worryLevel `mod` test m == 0
    newMonkey = removeHeadItem m
    (true, false) = if success
        then ([worryLevel], [])
        else ([], [worryLevel])
    in (newMonkey, true, false)

applyOperation :: Operation -> Int -> Int
applyOperation (Operation Mult (Number n)) x = x * n
applyOperation (Operation Plus (Number n)) x = x + n
applyOperation (Operation Mult Id) x = x * x
applyOperation (Operation Plus Id) x = x + x
