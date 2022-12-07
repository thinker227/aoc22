module Day07 where

import Solution (Solution (Separate), Answer)
import Control.Monad.State ( evalState, MonadState(state), State )
import ListUtils (startsWith)
import NumUtils (readNumToEnd)
import Data.List (sort)

day07 :: Solution
day07 = Separate part1 part2

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
    $ getDirectorySizes input

part2 :: String -> Answer
part2 input = let
    directorySizes = getDirectorySizes input
    rootSize = head directorySizes
    freeSpace = 70000000 - rootSize
    requiredSpace = 30000000 - freeSpace
    in show
        $ minimum
        $ filter (>= requiredSpace)
          directorySizes

getDirectorySizes :: String -> [Integer]
getDirectorySizes input =
      map getSize
    $ getSubDirs
    $ evalState readDir
    $ lines input

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

getSize (Directory _ files subDirs) =
    sum (map (toInteger . size) files) + sum (map getSize subDirs)
