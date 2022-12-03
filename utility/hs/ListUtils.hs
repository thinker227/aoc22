module ListUtils (
    splitBy,
    splitHalf,
    chunk,
    intersectAll
) where

import Data.List (intersect)

-- | Splits a list by a predicate.
-- Elements matching the predicate will act as separators
-- and will be discarded.
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f xs = splitBy' f xs [] []

splitBy' :: (a -> Bool) -> [a] -> [[a]] -> [a] -> [[a]]
splitBy' f (x:xs) l accum = if f x
    then splitBy' f xs (l ++ [accum]) []
    else splitBy' f xs l (accum ++ [x])
splitBy' _ _ l accum = l ++ [accum]

-- | Splits a list into two equal lists.
splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (length xs `div` 2) xs

-- | Chunks a list by a specified size,
-- producing a list of lists, each of which
-- is at most the specified size.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk len xs = take len xs : chunk len (drop len xs)

-- | Intersects all lists in a list.
intersectAll :: Eq a => [[a]] -> [a]
intersectAll (a:b:xs) = a `intersect` intersectAll (b:xs)
intersectAll [x] = x
intersectAll [] = []
