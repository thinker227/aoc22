module StateUtils where

import Control.Monad.State
import NumUtils (readNumToEnd)

-- | Reads the head element of the state.
readHead :: State [a] a
readHead = state f
    where f xs = (head xs, tail xs)

-- | Peeks the head element of the state
-- without modifying the state.
peekHead :: State [a] a
peekHead = state f
    where f xs = (head xs, xs)

-- | Reads a specified amount of elements from the state.
readMany :: Int -> State [a] [a]
readMany count = state f
    where f xs = splitAt count xs

-- | Reads a specified amount of elements from the state
-- without modifying the state.
peekMany :: Int -> State [a] [a]
peekMany count = state f
    where f xs = (take count xs, xs)

-- | Reads the proceeding elements of the state as an int.
readInt :: State String Int
readInt = state f
    where f xs = readNumToEnd xs

-- | Skips a specified amount of elements in the state.
skip :: Int -> State [a] ()
skip count = state f
    where f xs = ((), drop count xs)

-- | Tries to consume
consume :: Eq a => a -> State [a] (Maybe a)
consume x = do
    h <- peekHead
    if h == x
    then Just <$> readHead
    else pure Nothing

-- | Returns a state action which returns a constant value.
constState :: a -> State s a
constState x = state f
    where f s = (x, s)

-- | Runs a function on a state in a loop until a predicate on the state returns false.
-- Returns the values returned by the function in a list.
while :: State s Bool -> State s a -> State s [a]
while p f = do
    b <- p
    if not b
    then pure []
    else do
        x <- f
        xs <- while p f
        pure (x : xs)
