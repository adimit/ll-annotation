module Main where
    
import Tokenizer
import SimpleDTD
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

mkTokens :: [String] -> Tokens
mkTokens = Tokens . gentoks 0
         where gentoks :: Int -> [String] -> [Token]
               gentoks _ [] = []
               gentoks x (t:ts) = (Token attrs t) : gentoks (x+1) ts
                       where attrs = Token_Attrs { tokenIdx = "t" ++ show x }

prettyXML ::  Document () -> String
prettyXML = render . document 

main :: IO ()
main = do args <- getArgs
          case args of
               []    -> putStrLn "Please provide a file name."
               (x:_) -> do input <- C.readFile x
                           let xml = createXML $ tokenize' input
                           putStrLn $ prettyXML xml
