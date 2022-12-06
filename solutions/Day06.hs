module Day06 where
import Solution (Solution (Separate))
import Data.List (nub)

day06 = Separate
    (solve 4)
    (solve 14)

solve count input = show
    $ markerPos count
    $ zip input [0..length input]

markerPos count xs =
    let es = take count $ map fst xs
    in if nub es == es
        then snd (head xs) + count
        else markerPos count $ tail xs
