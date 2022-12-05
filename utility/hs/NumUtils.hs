module NumUtils where

import Data.Char (isNumber)

-- This function is purely to avoid having to write a type
-- annotation for every occurence of read.
readNum :: String -> Int
readNum = read

-- | Reads a number until an unreadable character is reached,
-- at which point the number is returned as well as the rest of the input.
readNumToEnd :: String -> (Int, String) 
readNumToEnd str = let
    numStr = takeWhile isNumber str
    in (readNum numStr, drop (length numStr) str)
