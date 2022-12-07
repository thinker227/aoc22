module ListUtils (
    splitBy,
    splitHalf,
    chunk,
    intersectAll,
    modifyAt,
    setAt,
    startsWith,
    valUnlessNull
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

-- | Returns a list with the element at a specified index
-- applied to a function.
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt pos f xs = let
    (a,b) = splitAt pos xs
    in a ++ f (head b) : tail b

-- | Returns a list with the element at a specified index
-- replaced with a new value.
setAt :: Int -> a -> [a] -> [a]
setAt pos x = modifyAt pos (const x)

-- | Returns whether a list starts with the elements of another list.
startsWith :: Eq a => [a] -> [a] -> Bool
startsWith xs pattern = take (length pattern) xs == pattern

-- | Returns a value if a list is not null, otherwise returns another.
valUnlessNull :: b -> b -> [a] -> b
valUnlessNull ifNotNull ifNull xs =
    if null xs
        then ifNull
        else ifNotNull
