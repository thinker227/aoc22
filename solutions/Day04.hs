module Day04 where

import Solution (Solution (Separate), Answer)
import NumUtils (readNum)
import Data.List (sortBy, intersect)
import ListUtils (splitBy)

day04 :: Solution
day04 = Separate
    (solve (\(xs, ys) -> all (`elem` ys) xs))
    (solve (\(xs, ys) -> not $ null $ xs `intersect` ys))

solve f input = show
    $ length
    $ filter (\[xs, ys] -> f (xs, ys))
    $ map (
          sortBy (\xs ys -> compare (length xs) (length ys))
        . map toRange
        . splitBy (== ','))
    $ lines input

toRange :: String -> [Int]
toRange str = let [a, b] = splitBy (== '-') str in
    [readNum a..readNum b]
