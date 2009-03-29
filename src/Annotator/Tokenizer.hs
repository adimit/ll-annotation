module Annotator.Tokenizer 
        (tokenize, tokenize') 
        where

import Prelude hiding (either)
import qualified Data.ByteString.Lazy.Char8 as C

-- Tokenize a ByteString to a list of ByteStrings.
tokenize :: C.ByteString -> [C.ByteString]
tokenize = C.groupBy token

-- Tokenize a ByteString to a list of Strings.
tokenize' :: C.ByteString -> [String]
tokenize' = map C.unpack . C.groupBy token

-- A helper function for tokenize's C.groupBy.
token :: Char -> Char -> Bool
token ' ' ' ' = True
token '\'' ' ' = False
token ' ' '\'' = False
token x y = not . or $ map (oneOf (x,y)) [' ','\t','\n','\r',',','.','!','?','\'','"','-']

-- Is one of (x,y) a z?
oneOf :: (Char,Char) -> Char -> Bool
oneOf (x,y) z | x == z  = True
              | y == z  = True
              | otherwise = False
