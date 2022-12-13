module Day13 (day13) where

import Solution(Solution(Single), Answer)
import ListUtils (splitBy)
import StringUtils (blank)
import StateUtils (peekHead, readInt, while, consume)
import Control.Monad.State (State, evalState, gets)

day13 :: Solution
day13 = Single part1

data Item
    = Value { value :: Int }
    | SubList { values :: [Item] }
    deriving (Show)

data Compare
    = Ordered
    | Unordered
    | Continue
    deriving (Eq, Show)

part1 :: String -> Answer
part1 input = show
    $ sum
    $ map fst
    $ filter snd
    $ zip [1..]
    $ map (
        (== Ordered)
      . uncurry ordered
      . (\[a, b] -> (a, b))
      . map (evalState parseList))
    $ splitBy blank
    $ lines input

ordered :: [Item] -> [Item] -> Compare
ordered [] [] = Continue
ordered [] _ = Ordered  -- Left ran out of items
ordered _ [] = Unordered -- Right ran out of items
ordered (l:ls) (r:rs) =
    let c = compareItems l r
    in if c == Continue
        then ordered ls rs
        else c

compareItems :: Item -> Item -> Compare
compareItems (Value l) (Value r) = case compare l r of
    LT -> Ordered
    EQ -> Continue
    GT -> Unordered
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
