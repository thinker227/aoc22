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
    $ map (
        uncurry ordered
      . (\[a, b] -> (a, b))
      . map (evalState parseList))
    $ splitBy blank
    $ lines input

ordered :: [Item] -> [Item] -> Bool
ordered [] [] = True
ordered [] _ = True  -- Left ran out of items
ordered _ [] = False -- Right ran out of items
ordered (l:ls) (r:rs) = compareItems l r && ordered ls rs

compareItems :: Item -> Item -> Bool
compareItems (Value l) (Value r) = l <= r
compareItems (SubList l) (SubList r) = ordered l r
compareItems l (SubList r) = ordered [l] r
compareItems (SubList l) r = ordered l [r]

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
