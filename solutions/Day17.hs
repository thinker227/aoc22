module Day17 where

import Solution(Solution(Single), Answer)
import Position(Pos(Pos, ypos, xpos))
import HashSetUtils(insertAll)
import Prelude hiding (Left, Right)
import Data.HashSet(HashSet, fromList, member, empty)

day17 :: Solution
day17 = Single part1

data World = World {
    rocks :: HashSet (Pos Int),
    top :: Int,
    jets :: [Jet] }
instance Show World where
    show (World rs t _) = "rocks = " ++ show rs ++ ", top = " ++ show t
data Jet = Left | Right
    deriving (Show)
type Rock = [Pos Int]

part1 :: String -> Answer
part1 input = let
    jets = repeatJets $ parse input
    in show
    $ foldl (\w r -> fallJ w (moveRockAtStart w r)) (World empty 0 jets) (take 2022 rockSeq)

parse = map (\c -> if c == '<' then Left else Right)

moveRockAtStart :: World -> Rock -> Rock
moveRockAtStart w r = move r (Pos 2 (top w + 3))

repeatJets js = js ++ repeatJets js

rockKinds = [
    [Pos 0 0, Pos 1 0, Pos 2 0, Pos 3 0],
    [Pos 1 2, Pos 0 1, Pos 1 1, Pos 2 1, Pos 1 0],
    [Pos 2 2, Pos 2 1, Pos 2 0, Pos 1 0, Pos 0 0],
    [Pos 0 3, Pos 0 2, Pos 0 1, Pos 0 0],
    [Pos 0 1, Pos 1 1, Pos 0 0, Pos 1 0]
    ]

rockSeq :: [Rock]
rockSeq = rockKinds ++ rockSeq

fallJ :: World -> Rock -> World
fallJ (World rs t (j:js)) r = let
    d = dir j
    w = World rs t js
    in if canMove rs r d
        then fallD w (move r d) -- Jet moved the rock
        else fallD w r -- Jet did not move the rock
fallJ _ _ = error "No more jets (unreachable)"

fallD :: World -> Rock -> World
fallD (World rs t js) r =
    let d = Pos 0 (-1)
    in if canMove rs r d
        then fallJ (World rs t js) (move r d)
        else addToWorld (World rs t js) r -- Could not fall further

dir Left = Pos (-1) 0
dir Right = Pos 1 0

addToWorld :: World -> Rock -> World
addToWorld (World rs t js) r =
    let top' = max t (maximum $ map ypos r)
    in World (insertAll r rs) top' js

move :: Rock -> Pos Int -> Rock
move r p = map (+p) r

canMove rs r d =
    all (f . (+d)) r
    where f p =
            not (member p rs) &&
            xpos p > (-1) &&
            xpos p < 7 &&
            ypos p > (-1)

height :: Rock -> Int
height r = let ys = map ypos r
    in maximum ys - minimum ys + 1
