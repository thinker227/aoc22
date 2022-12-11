module StringUtils where
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- | Returns `True` for a string which only contains whitespace.
blank :: String -> Bool
blank = all isSpace

-- | Trims whitespace from the beginning and end of a string.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
