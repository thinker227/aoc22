module Day10 where

import Solution (Solution(Separate), Answer)
import NumUtils (readNumToEnd)
import ListUtils (chunk, join)
import Control.Monad.State ( gets, modify, evalState, MonadState(state), State )

day10 :: Solution
day10 = Separate part1 part2

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
    execs = getExecs input
    cycles = [20, 60, 100, 140, 180, 220]
    in show
    $ sum
    $ map (
          (\(Exec reg cyc) -> reg * cyc)
        . (\x -> execs !! (x - 1)))
      cycles

part2 :: String -> Answer 
part2 input = ("\n" ++)
    $ concat
    $ join "\n"
    $ chunk 40
    $ map screenChar
    $ init
    $ getExecs input

screenChar :: Exec -> Char
screenChar (Exec reg cyc) =
    if ((cyc - 1) `mod` 40) `elem` [reg - 1 .. reg + 1]
    then '#'
    else '.'

getExecs :: String -> [Exec]
getExecs input =
    evalState execAll (ExecState (lines input ++ ["noop"]) 1 1)

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
