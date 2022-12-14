module Day14 where

import Solution (Solution(Single), Answer)
import ListUtils (splitByMany, view, splitBy, firstOrNothing)
import NumUtils (readNum)
import Position (Pos(Pos, ypos), posJoin)
import Data.List (nub)

day14 :: Solution
day14 = Single part1

data Cave = Cave {
    rocks :: [Pos Int],
    sand :: [Pos Int] }
    deriving (Show)

part1 :: String -> Answer
part1 input = let
    cave = Cave (parse input) []
    in show
    $ length
    $ sand
    $ fallAll cave

fallAll :: Cave -> Cave
fallAll cave = maybe cave fallAll (fall cave (Pos 500 0))

fall :: Cave -> Pos Int -> Maybe Cave
fall cave pos = let
    r = rocks cave
    s = sand cave
    dist = firstOrNothing (\p -> fallable cave (pos + p)) fallPatterns
    in case fmap (+ pos) dist of
        Nothing -> Just (Cave r (pos : s))
        Just p -> if ypos p <= maximum (map ypos r)
            then fall cave p
            else Nothing

fallable (Cave rocks sand) pos = pos `notElem` rocks && pos `notElem` sand

fallPatterns = [Pos 0 1, Pos (-1) 1, Pos 1 1]

parse input = nub
    $ concatMap (
          concatMap (\[a, b] -> line a b)
        . view 2
        . map parsePos
        . splitByMany " -> ")
    $ lines input

parsePos str = let
    [x, y] = splitBy (== ',') str
    in Pos (readNum x) (readNum y)

line a b = let
    m = b - a
    dir = signum m
    dist = abs (posJoin (+) m)
    in map (\i -> a + fmap (*i) dir) [0..dist]
