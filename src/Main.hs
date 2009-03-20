module Main where

import Tokenizer
import Control.Monad (forM)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do args <- getArgs
          case args of
               []    -> putStrLn "Please provide a file name."
               (x:_) -> do input <- C.readFile x
                           let tokens = tokenize' input
                           putStrLn $ show $ map C.unpack tokens

