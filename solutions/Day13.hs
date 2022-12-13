module Day13 where

import Solution(Solution(Single), Answer)
import ListUtils (splitBy)
import StringUtils (blank)
import StateUtils
import Control.Monad.State (State, evalState, MonadState (get), gets)

day13 :: Solution
day13 = Single part1

data Item
    = Value { value :: Int }
    | SubList { values :: [Item] }
    deriving (Show)

part1 :: String -> Answer
part1 input = show
    $ map ((\[a, b] -> (a, b)) . map (evalState parseList))
    $ splitBy blank
    $ lines input

parseList :: State String [Item]
parseList = do
    consume '[' -- Skip the leading '['

    item <- while
        (gets (\s -> not (null s || head s == ']')))
        (do head <- peekHead
            x <- if head == '['
                then SubList <$> parseList
                else Value <$> readInt
            consume ',' -- Skip the separating ','
            pure x)

    consume ']' -- Skip the trailing ']'
    pure item
