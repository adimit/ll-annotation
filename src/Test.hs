module Main where
    
import Tokenizer
import SimpleDTD
import Control.Monad (forM)
import System.Environment (getArgs)
import Text.XML.HaXml.XmlContent (toXml)
import qualified Data.ByteString.Lazy.Char8 as C

mkTokens :: [String] -> Tokens
mkTokens = Tokens . gentoks 0
         where gentoks :: Int -> [String] -> [Token]
               gentoks _ [] = []
               gentoks x (t:ts) = (Token attrs t) : gentoks (x+1) ts
                       where attrs = Token_Attrs { tokenIdx = "t" ++ show x }

main :: IO ()
main = do args <- getArgs
          case args of
               []    -> putStrLn "Please provide a file name."
               (x:_) -> do input <- C.readFile x
                           let tokens = tokenize input
                           putStrLn $ show $ tokens

