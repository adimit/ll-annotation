module Tokenizer 
        (tokenize) 
        where

import Prelude hiding (either)
import qualified Data.ByteString.Lazy.Char8 as C


tokenize :: C.ByteString -> [String]
tokenize = map C.unpack . C.groupBy token

token :: Char -> Char -> Bool
token ' ' ' ' = True
token '\'' ' ' = False
token ' ' '\'' = False
token x y = not . or $ map (oneOf (x,y)) [' ',',','.','!','?','\'','"','-']

oneOf :: (Char,Char) -> Char -> Bool
oneOf (x,y) z | x == z  = True
              | y == z  = True
              | otherwise = False
