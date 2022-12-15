module RangeUtils(
    Range,
    rIntersectAll,
    rIntersect,
    rIntersecting,
    rLength
) where

-- | A range of values.
type Range a = (a, a)

-- | Intersects all ranges in a list.
rIntersectAll :: Ord a => [Range a] -> [Range a]
rIntersectAll = rIntersectAll' []

rIntersectAll' :: Ord a => [Range a] -> [Range a] -> [Range a]
rIntersectAll' acc (x:y:xs) = let
    ms = rIntersect x y
    in if length ms == 1
        then rIntersectAll' [] (acc ++ ms ++ xs)
        else rIntersectAll' (x:acc) (y:xs)
rIntersectAll' acc xs = xs ++ acc

-- | Intersects two ranges. If the intersection succeeded,
-- returns a list of the single produced range,
-- otherwise returns a list of the two input ranges.
rIntersect :: Ord a => Range a -> Range a -> [Range a]
rIntersect (a, b) (c, d) = if rIntersecting (a, b) (c, d)
        then [(minimum [a, b, c, d], maximum [a, b, c, d])]
        else [(a, b), (c, d)]

-- | Determines whether two ranges are intersecting.
rIntersecting :: Ord a => Range a -> Range a -> Bool
rIntersecting (a, b) (c, d) =
    (a >= c && a <= d) ||
    (b >= c && b <= d) ||
    (c >= a && c <= b) ||
    (d >= a && d <= b)

-- | Gets the length of a range.
rLength :: (Num a, Ord a) => Range a -> a
rLength (a, b) = max a b - min a b + 1
