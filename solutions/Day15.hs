module Day15 where

import Solution(Solution(Single), Answer)
import Position(Pos(Pos, xpos, ypos), posCombine, posJoin)
import ListUtils(remove)
import StateUtils(skip, readInt)
import RangeUtils(rIntersectAll, rLength, Range)
import Control.Monad.State(State, evalState)
import Data.List(nub)
import Data.Maybe(mapMaybe)

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
    $ sum
    $ map rLength
    $ rIntersectAll
    $ cellCount y
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

cellCount :: Int -> [Sensor] -> [Range Int]
cellCount y = mapMaybe (cellRangeAtY y)

cellRangeAtY :: Int -> Sensor -> Maybe (Range Int)
cellRangeAtY y s = let
    cellCount = cellCountAtY s y
    xmin = xpos (pos s) - (cellCount `div` 2)
    in if cellCount > 0
        then Just (xmin, xmin + cellCount - 1)
        else Nothing

cellCountAtY s y = cellCountAtOffset (radius s) (y - ypos (pos s))

-- n = max 0 (2r + 1 - 2(|off|))
cellCountAtOffset r off = max 0 (2 * r + 1 - 2 * abs off)
