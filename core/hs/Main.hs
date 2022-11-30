module Main where

import Solution (Solution (Single, Separate, Combined), runSolution, Answer)
import Data.Map ( (!?), fromList, Map )
import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Directory ( doesFileExist )

days :: Map Int Solution
days = fromList
    [ (-1, Single id),
      (0, Combined (const ("Test part 1", "Test part 2")))
    ]

getDaySolution :: String -> Maybe Solution
getDaySolution day = do
    dayNum <- readMaybe day
    days !? dayNum

getInput :: String -> IO String
getInput str = do
    fileExists <- doesFileExist str
    if fileExists
    then readFile str
    else pure str

main = do
    args <- getArgs
    if length args < 2
    then putStrLn usage
    else do
        let (day : inputStr : _) = args
        let solution = getDaySolution day
        input <- getInput inputStr
        case solution of
            Nothing -> putStrLn $ "Unknown day " ++ day
            Just so -> putStrLn $ getAnswerString $ runSolution so input

getAnswerString (p1, p2) =
    "Part 1: " ++ p1 ++ case p2 of
        Nothing -> ""
        Just s -> "\nPart 2: " ++ s

usage =
    "Usage: <day> <input>\n" ++
    "\n" ++
    "Arguments:\n" ++
    "<day>       The day to run the solver for.\n" ++
    "<input>     The input to the solver,\n" ++
    "            or the file path to a file\n" ++
    "            containing the input."
