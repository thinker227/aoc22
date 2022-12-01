module StringUtils where
import Data.Char (isSpace)

blank :: String -> Bool
blank = all isSpace
