module HashSetUtils(
    insertAll
) where

import Data.HashSet
import Data.Hashable

insertAll :: (Eq a, Hashable a) => [a] -> HashSet a -> HashSet a
insertAll xs h = Prelude.foldr insert h xs
