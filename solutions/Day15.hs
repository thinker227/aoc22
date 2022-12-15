module Day15 where

import Solution(Solution(Single), Answer)
import Position(Pos(Pos, xpos, ypos), posCombine, posJoin)
import StateUtils(skip, readInt)
import Control.Monad.State(State, evalState)
import Data.List (nub)
import ListUtils (remove)

day15 :: Solution
day15 = Single (part1 2000000)

data Sensor = Sensor {
    pos :: Pos Int,
    beacon :: Pos Int }
    deriving (Show)

part1 y input = let
    sensors = map (evalState parse) (lines input)
    beacons = nub (map beacon sensors)
    beaconsAtY = filter ((== y) . ypos) beacons
    in show
    $ (\x -> x - length beaconsAtY)
    $ length
    $ nub
    $ concatMap (`cellsAtY` y)
      sensors

-- Returns (sensor position, sensor radius)
parse :: State String Sensor
parse = do
    skip 12
    sx <- readInt
    skip 4
    sy <- readInt
    skip 25
    bx <- readInt
    skip 4
    by <- readInt

    let s = Pos sx sy
    let b = Pos bx by

    pure $ Sensor s b

manhattan a b = posJoin (+) $ posCombine (\a b -> abs (a - b)) a b

radius (Sensor s b) = manhattan s b

cellsAtY s y = let
    cellCount = cellCountAtY s y
    xmin = xpos (pos s) - (cellCount `div` 2)
    in map (`Pos` y) [xmin .. xmin + cellCount - 1]

cellCountAtY s y = cellCountAtOffset (radius s) (y - ypos (pos s))

-- n = max 0 (2r + 1 - 2(|off|))
cellCountAtOffset r off = max 0 (2 * r + 1 - 2 * abs off)
