module NumUtils (
    readNum,
    readNumToEnd
) where

import Data.Char (isNumber, digitToInt)

-- | Reads a number from a string. Supports negative signs.
readNum :: String -> Int
readNum str = readNum' str 0

readNum' :: String -> Int -> Int
readNum' ('-':cs) 0 = -(readNum' cs 0)
readNum' (c:cs) acc = readNum' cs (acc * 10 + digitToInt c)
readNum' [] acc = acc

-- | Reads a number until an unreadable character is reached,
-- at which point the number is returned as well as the rest of the input.
readNumToEnd :: String -> (Int, String) 
readNumToEnd str = let
    numStr = takeWhile (\c -> isNumber c || c == '-') str
    in (readNum numStr, drop (length numStr) str)
