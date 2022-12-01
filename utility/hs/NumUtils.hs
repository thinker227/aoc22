module NumUtils where

-- This function is purely to avoid having to write a type
-- annotation for every occurence of read.
readNum :: String -> Int
readNum = read
