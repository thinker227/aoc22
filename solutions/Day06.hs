module Day06 (day06) where

import Solution (Solution (Separate))
import Data.List (nub)

day06 = Separate
    (solve 4)
    (solve 14)

solve count input = show
    $ (+ count)
    $ head
    $ filter (\x ->
        let es = take count $ drop x input
        in nub es == es)
      [0 .. length input]
