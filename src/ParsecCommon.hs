-- HsParser: A Parsec builder, a toy for experimenting things:
-- @2013 Angel Alvarez, Felipe Zapata, from The ResMol Group  

-- Common types to use with Parsec 3.0 style parsers 
module ParsecCommon where

import Data.Functor.Identity
import Text.Parsec

-- A parsecT based parser carring state of type "a" and returning data of type "b"
type MyParser a b  = ParsecT [Char] a Identity b

