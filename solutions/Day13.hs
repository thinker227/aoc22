module Day13 (day13) where

import Solution(Solution(Separate), Answer)
import ListUtils (splitBy, join)
import StringUtils (blank)
import StateUtils (peekHead, readInt, while, consume)
import Control.Monad.State (State, evalState, gets)
import Data.List (sortBy)

day13 :: Solution
day13 = Separate part1 part2

data Item
    = Value { value :: Int }
    | SubList { values :: [Item] }
    deriving (Eq)

data Compare
    = Ordered
    | Unordered
    | Continue
    deriving (Eq)

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

part2 :: String -> Answer
part2 input = show
    $ product
    $ map fst
    $ filter ((`elem` drivers) . snd)
    $ zip [1..]
    $ sortBy (\l r -> if ordered l r == Ordered then LT else GT)
    $ (++ drivers)
    $ concatMap (map (evalState parseList))
    $ splitBy blank
    $ lines input

drivers :: [[Item]]
drivers = [[SubList [Value 2]], [SubList [Value 6]]]

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
