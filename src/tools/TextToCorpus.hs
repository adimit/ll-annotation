module Main where

import Annotator.Tokenizer
import Annotator.DTD
import System.Environment (getArgs)
import Text.PrettyPrint.HughesPJ (render)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml.XmlContent (toXml)
import qualified Data.ByteString.Lazy.Char8 as C

-- Takes a set of tokens as Strings and transforms them to an XML document
createXML :: [String] -> Document ()
createXML xs = toXml False (Corpus tokens (Errors []))
        where tokens = mkTokens xs

-- Take tokenized text and turn it into a <tokens/> element
mkTokens :: [String] -> Tokens
mkTokens toks = Tokens (Tokens_Attrs {tokensAmount = show $ length toks} ) (gentoks 0 toks)
         where gentoks :: Int -> [String] -> [Token]
               gentoks _ [] = []
               gentoks x (t:ts) = (Token attrs t) : gentoks (x+1) ts
                       where attrs = Token_Attrs { tokenIdx = "t" ++ show x }

-- render an XML document
prettyXML ::  Document () -> String
prettyXML = render . document

main :: IO ()
main = do args <- getArgs
          case args of
               []    -> putStrLn "Please provide a file name."
               (x:_) -> do input <- C.readFile x
                           let xml = createXML $ tokenize' input
                           putStrLn $ prettyXML xml
