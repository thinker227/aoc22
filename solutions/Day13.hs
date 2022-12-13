module Day13 where

import Solution(Solution(Single), Answer)
import ListUtils (splitBy)
import StringUtils (blank)
import StateUtils
import Control.Monad.State (State, evalState)

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
    skip 1 -- Skip the leading '['
    
    skip 1 -- Skip the trailing ']'
    error "Not implemented"
