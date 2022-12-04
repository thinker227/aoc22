module Day04 where

import Solution (Solution (Separate), Answer)
import NumUtils (readNum)
import ListUtils (splitBy)
import Data.List (sortBy, intersect)

day04 = Separate
    -- Check for full intersection
    (solve (\(xs, ys) -> all (`elem` ys) xs))
    -- Check for any intersection
    (solve (\(xs, ys) -> not $ null $ xs `intersect` ys))

solve f input = show
    $ length
    $ filter (\[xs, ys] -> f (xs, ys))
    $ map (
          sortBy (\xs ys -> compare (length xs) (length ys))
        . map toRange
        . splitBy (== ','))
    $ lines input

toRange str = let [a, b] = splitBy (== '-') str in
    [readNum a..readNum b]
