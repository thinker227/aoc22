module Day10 where

import Solution (Solution(Single), Answer)
import Control.Monad.State
import NumUtils (readNumToEnd)

day10 :: Solution
day10 = Single part1

data Exec = Exec {
    register :: Int,
    cycle :: Int }
    deriving (Show)
data ExecState = ExecState {
    ops :: [String],
    registerVal :: Int,
    cycleVal :: Int }

part1 :: String -> Answer
part1 input = let
    ops = lines input ++ ["noop"]
    execs = evalState execAll (ExecState ops 1 1)
    cycles = [20, 60, 100, 140, 180, 220]
    in show
    $ sum
    $ map (
          (\(Exec reg cyc) -> reg * cyc)
        . (\x -> execs !! (x - 1)))
      cycles

readLine :: State ExecState String
readLine = state f
    where f (ExecState ops reg cyc) = (head ops, ExecState (tail ops) reg cyc)

execAll :: State ExecState [Exec]
execAll = do
    ls <- gets ops

    if null ls
    then pure []
    else do
        e <- exec
        es <- execAll
        pure (e ++ es)

exec :: State ExecState [Exec]
exec = do
    l <- readLine

    let op = take 4 l
    let (n, _) = readNumToEnd (drop 5 l)
    reg <- gets registerVal
    cyc <- gets cycleVal

    let (cycles, cycleCount) = if op == "addx"
        then ([Exec reg cyc, Exec reg (cyc + 1)], 2)
        else ([Exec reg cyc], 1)

    let newReg = reg + n
    let newCyc = cyc + cycleCount
    modify (\(ExecState ops _ _) -> ExecState ops newReg newCyc)

    pure cycles
