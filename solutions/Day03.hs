module Day03 (day03) where

import Solution (Solution (Separate, Single), Answer)
import ListUtils (splitHalf, chunk, intersectAll)
import Data.List (intersect, nub)
import Data.Char (ord)

day03 = Separate
    -- Take all lines, split them in half, then intersect the halves
    (solve id (uncurry intersect . splitHalf))
    -- Chunk all lines by 3, then intersect them
    (solve (chunk 3) intersectAll)

solve fa fb input = show
    $ sum
    $ map (priority . head . nub . fb)
    $ fa
    $ lines input

priority c = let n = ord c in
    if c `elem` ['a'..'z']
    then n - ord 'a' + 1
    else (n - ord 'A') + 27
