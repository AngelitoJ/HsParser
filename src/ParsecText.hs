module ParsecText where

import Data.Char
-- import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import ParsecCommon


--oneOfStrings :: [String] -> GenParser Char st String
oneOfStrings :: [String] -> MyParser st String
oneOfStrings listOfStrings = choice $ map (try . string) listOfStrings

-- Parse a text line as a whole into a string
anyLine :: MyParser st String
anyLine = manyTill anyChar newline     -- whatever chars we find till we hit a newline

-- Parser a line containing some string
stringLine :: String -> MyParser st String
stringLine str = do
    spaces
    result <- string str
    manyTill anyChar newline
    return result

blankLines :: MyParser st [String]
blankLines = many1 blankLine

-- Parse a blank line as a whole into a string
blankLine :: MyParser st String
blankLine = manyTill space newline     -- whatever spaces we find till we hit a newline
