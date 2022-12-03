module StringUtils where
import Data.Char (isSpace)

-- | Returns `True` for a string which only contains whitespace.
blank :: String -> Bool
blank = all isSpace
