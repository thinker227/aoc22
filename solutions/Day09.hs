module Day09 where

import Solution (Solution(Single), Answer)
import Position (Pos(Pos), zero)
import NumUtils (readNum)
import Control.Monad.State (State, MonadState(state), evalState, gets, modify)
import Data.List (nub)

day09 :: Solution
day09 = Single part1

-- This is just to keep unit positions which represent
-- motions separate from non-unit positions
type Motion = Pos Integer
data TraceState = TraceState {
    motions :: [Motion],
    headPos :: Pos Integer,
    tailPos :: Pos Integer }

part1 :: String -> Answer
part1 input = let
    motions = concatMap motion $ lines input
    in show
    $ length
    $ nub
    $ evalState traceMotions (TraceState motions zero zero)

motion :: String -> [Motion]
motion (dir:_:numStr) = let
    num = readNum numStr
    m = case dir of
        'R' -> Pos 1 0
        'L' -> Pos (-1) 0
        'D' -> Pos 0 1
        'U' -> Pos 0 (-1)
        _ -> error "Malformed input"
    in replicate num m
motion _ = error "Malformed input"

nextMotion :: State TraceState Motion
nextMotion = state f
    where f (TraceState motions headPos tailPos) =
            (head motions, TraceState (tail motions) headPos tailPos)

traceMotions :: State TraceState [Pos Integer]
traceMotions = do
    ms <- gets motions

    -- If there are no more motions, then end the recursion
    if null ms
    then pure []

    else do
        -- Trace a motion
        t <- traceMotion
        -- Trace the rest
        ts <- traceMotions

        pure (t : ts)

traceMotion :: State TraceState (Pos Integer)
traceMotion = do
    m <- nextMotion
    head <- gets headPos
    tail <- gets tailPos

    -- Set the new head position in the state
    let newHead = head + m
    modify (\(TraceState motions _ tail) -> TraceState motions newHead tail)

    -- The distance between the head and the tail
    let dist = newHead - tail
    if moveTail dist
    then do
        -- Set the new tail to the old head position
        let newTail = head
        modify (\(TraceState motions head _) -> TraceState motions head newTail)

        -- Return the new tail
        pure newTail
    else
        -- Return the old tail
        pure tail

moveTail :: Motion -> Bool
moveTail (Pos (-1) 2) = True
moveTail (Pos 0 2) = True
moveTail (Pos 1 2) = True
moveTail (Pos 2 1) = True
moveTail (Pos 2 0) = True
moveTail (Pos 2 (-1)) = True
moveTail (Pos 1 (-2)) = True
moveTail (Pos 0 (-2)) = True
moveTail (Pos (-1) (-2)) = True
moveTail (Pos (-2) (-1)) = True
moveTail (Pos (-2) 0) = True
moveTail (Pos (-2) 1) = True
moveTail _ = False
