module ListUtils (
    splitBy,
    splitByMany,
    splitHalf,
    chunk,
    view,
    intersectAll,
    modifyAt,
    setAt,
    startsWith,
    valUnlessNull,
    aheadTail,
    columns,
    join,
    for,
    first,
    firstOrNothing
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

-- | Splits a list by a sublist.
-- Sequences of elements matching the sublist will act as separators
-- and will be discarded.
splitByMany :: Eq a => [a] -> [a] -> [[a]]
splitByMany f xs = splitByMany' f xs [] []

splitByMany' :: Eq a => [a] -> [a] -> [[a]] -> [a] -> [[a]]
splitByMany' [] xs _ _ = map (: []) xs
splitByMany' _ [] l accum = l ++ [accum]
splitByMany' e xs l accum = let
    len = length e
    es = take len xs
    in if es == e
        then splitByMany' e (drop len xs) (l ++ [accum]) []
        else splitByMany' e (tail xs) l (accum ++ [head xs])

-- | Splits a list into two equal lists.
splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (length xs `div` 2) xs

-- | Chunks a list by a specified size,
-- producing a list of lists, each of which
-- is at most the specified size.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk len xs = take len xs : chunk len (drop len xs)

-- | Produces a list of sublists, each of which is
-- exactly a specified length and represents a sequential
-- view over the source list.
view :: Int -> [a] -> [[a]]
view _ [] = []
view len xs = if length xs >= len
    then take len xs : view len (tail xs)
    else []

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

-- | For every element of a list, returns a tuple containing
-- the preceeding elements, the current element, and the proceeding elements.
aheadTail :: [a] -> [([a], a, [a])]
aheadTail xs = map (\i -> (take i xs, xs !! i, drop (i + 1) xs)) [0 .. length xs - 1]

columns :: [[a]] -> [[a]]
columns ([]:_) = []
columns xs = map head xs : columns (map tail xs)

join :: a -> [a] -> [a]
join _ [x] = [x]
join sep (x:xs) = x : sep : join sep xs
join _ _ = []

-- | Returns a list which is the result of applying a function
-- to the preceeding element a specific amount of times.
for :: (a -> a) -> Int ->  a -> [a]
for f max x = for' 0 x max f

for' :: Int -> a -> Int -> (a -> a) -> [a]
for' i x max f = if i >= max
    then []
    else let x' = f x
        in x' : for' (i + 1) x' max f

-- | Returns the first element of a list which matches a predicate.
first :: (a -> Bool) -> [a] -> a
first f = head . filter f

firstOrNothing :: (a -> Bool) -> [a] -> Maybe a
firstOrNothing _ [] = Nothing
firstOrNothing f (x:xs) = if f x
    then Just x
    else firstOrNothing f xs
