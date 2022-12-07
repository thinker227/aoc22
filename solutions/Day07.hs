module Day07 where

import Solution (Solution (Single), Answer)
import Control.Monad.State ( evalState, MonadState(state), State )
import ListUtils (startsWith)
import NumUtils (readNumToEnd)

day07 :: Solution
day07 = Single part1

data Directory = Directory {
    dirName :: String,
    files :: [File],
    subDirs :: [Directory] }
    deriving (Show, Eq)
data File = File {
    fileName :: String,
    size :: Int }
    deriving (Show, Eq)

part1 :: String -> Answer
part1 input = show
    $ sum
    $ filter (<= 100000)
    $ map getSize
    $ getSubDirs
    $ evalState readDir
    $ lines input

-- readLines :: State [String] [String]
-- readLines = state f
--     where f ls = ()

readLine :: State [String] String
readLine = state f
    where f ls = if null ls
            then ([], [])
            else (head ls, tail ls)

peekLine :: State [String] String
peekLine = state f
    where f ls = if null ls
            then ([], [])
            else (head ls, ls)

readEntries :: State [String] [String]
readEntries = state f
    where f ls =
            let xs = takeWhile (\x -> head x /= '$') ls
            in (xs, drop (length xs) ls)

toFile str =
    let (n, s) = readNumToEnd str
    in File (tail s) n

readDir :: State [String] Directory
readDir = do
    cd <- readLine
    let name = drop 5 cd

    _ <- readLine -- Swallow "$ ls" command

    entries <- readEntries
    let files =
            map toFile $
            filter (not . (`startsWith` "dir")) entries

    subDirs <- readSubDirs

    _ <- readLine -- Swallow "$ cd .." command

    pure $ Directory name files subDirs

readSubDirs :: State [String] [Directory]
readSubDirs = do
    l <- peekLine
    if (l == "$ cd ..") || (l == "")
        then pure []
        else do
            dir <- readDir
            next <- readSubDirs
            pure $ dir : next

getSubDirs dir =
    dir : concatMap getSubDirs (subDirs dir)

getSize :: Directory -> Integer
getSize (Directory _ files subDirs) =
    sum (map (toInteger . size) files) + sum (map getSize subDirs)
