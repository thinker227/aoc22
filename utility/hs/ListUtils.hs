module ListUtils (splitBy, splitHalf, chunk, intersectAll) where

import Data.List (intersect)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f xs = splitBy' f xs [] []

splitBy' :: (a -> Bool) -> [a] -> [[a]] -> [a] -> [[a]]
splitBy' f (x:xs) l accum = if f x
    then splitBy' f xs (l ++ [accum]) []
    else splitBy' f xs l (accum ++ [x])
splitBy' _ _ l accum = l ++ [accum]

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (length xs `div` 2) xs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk len xs = take len xs : chunk len (drop len xs)

intersectAll :: Eq a => [[a]] -> [a]
intersectAll (a:b:xs) = a `intersect` intersectAll (b:xs)
intersectAll [x] = x
intersectAll [] = []
