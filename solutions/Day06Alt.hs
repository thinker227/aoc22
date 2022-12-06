module Day06Alt (day06Alt) where

import Solution (Solution (Separate))
import Data.List (nub)

day06Alt = Separate
    (solve 4)
    (solve 14)

solve count input = show
    $ (+ count)
    $ head
    $ filter (\x -> length (nub $ take count $ drop x input) == count)
      [0..length input]
