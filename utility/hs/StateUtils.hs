module StateUtils where

import Control.Monad.State
import NumUtils (readNumToEnd)

readHead :: State [a] a
readHead = state f
    where f xs = (head xs, tail xs)

peekHead :: State [a] a
peekHead = state f
    where f xs = (head xs, xs)

readMany :: Int -> State [a] [a]
readMany count = state f
    where f xs = splitAt count xs

peekMany :: Int -> State [a] [a]
peekMany count = state f
    where f xs = (take count xs, xs)

readInt :: State String Int
readInt = state f
    where f xs = readNumToEnd xs

skip :: Int -> State [a] ()
skip count = state f
    where f xs = ((), drop count xs)

while :: (s -> Bool) -> (s -> State s a) -> State s [a]
while p f = do
    s <- get
    if not (p s)
    then pure []
    else do
        m <- f s
        ms <- while p f
        pure (m : ms)
