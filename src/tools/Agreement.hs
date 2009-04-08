module Main where

import Annotator.DTD
import Annotator.Interface.Util
import Control.Monad
import Data.Either
import Data.List
import System.Environment

import Text.XML.HaXml.XmlContent.Haskell (readXml)

data ComparedRecords = ComparedRecords { agreement    :: [RecordPair]
                                       , disagreement :: ([Record],[Record]) } 
                                       deriving (Show, Eq)
data RecordPair = RecordPair Record Record deriving (Show, Eq)

main :: IO ()
main = do args <- getArgs
          case args of
              (_:_:_) -> treatFiles args
              _       -> putStrLn "Usage: agreement a1.xml a2.xml"

treatFiles :: [FilePath] -> IO ()
treatFiles files = do parses <- forM files (\f -> fmap readXml (readFile f))
                      case lefts parses of
                           [] -> compareCorpora $ rights parses
                           errors -> do forM_ errors putStrLn


compareCorpora :: [Corpus] -> IO ()
compareCorpora ((Corpus (Tokens a1 ts1) (Errors es1)):(Corpus (Tokens a2 ts2) (Errors es2)):[]) = do
    putStrLn "Testing for token to token Consistency..."
    if a1 /= a2 -- || ts1 /= ts2
       then putStrLn "Tokens inconsistent. Checking agreement doesn't make sense this way."
       else do putStrLn "Tokens consistent."
               compareErrors2 es1 es2
compareCorpora _          = putStrLn "Sorry, but only two corpora can be evaluated at this point."

compareErrors2 :: [Record] -> [Record] -> IO ()
compareErrors2 es1 es2 = do
        printColumn2 "Annotations" (show $ length es1) (show $ length es2)
        printColumn "Total agreement" (show . length $ intersectBy recordIdentity es1 es2)
        printColumn "Token agreement" (show . length $ intersectBy tokenIdentity es1 es2)
        printColumn "token + subtree" (show . length $ intersectBy tokenSubtree es1 es2)
        printColumn "token subset" (show . length $ intersectBy tokenSubset es1 es2)
        printColumn "subset subtree" (show . length $ intersectBy subsetSubtree es1 es2)
        printColumn "nonmatches" (show . length $ intersectBy (not subsetSubtree) es1 es2)

recordIdentity :: Record -> Record -> Bool
recordIdentity l r = tokenIdentity l r && errortype l == errortype r

tokenIdentity :: Record -> Record -> Bool
tokenIdentity l r = errtoks l == errtoks r

tokenSubtree  :: Record -> Record -> Bool
tokenSubtree  l r = tokenIdentity l r && subtreeIdentity (errortype l) (errortype r)

tokenSubset   :: Record -> Record -> Bool
tokenSubset   l r = intersect (errtoks l) (errtoks r) /= []

subsetSubtree :: Record -> Record -> Bool
subsetSubtree l r = tokenSubset l r && subtreeIdentity (errortype l) (errortype r)

subtreeIdentity :: [String] -> [String] -> Bool
subtreeIdentity [] _  = True
subtreeIdentity _  [] = True
subtreeIdentity (sl:ssl) (sr:ssr) =  sl == sr || subtreeIdentity ssl ssr

printColumn :: String -> String -> IO ()
printColumn name s1 = do putStr $ staticString 15 name
                         putStr $ ": "
                         putStrLn $ (staticString 10 s1)

printColumn2 :: String -> String -> String -> IO ()
printColumn2 name s1 s2 = do putStr $ staticString 15 name
                             putStr $ ": "
                             putStrLn $ (staticString 10  s1) ++ " | " ++ (staticString 10 s2)


staticString :: Int -> String -> String
staticString n s | (length s) < n = replicate (n - length s) ' ' ++ s
                 | (length s) > n = take (n - 3) s ++ "..."
                 | otherwise = s

errtoks :: Record -> [String]
errtoks (Record _ (Errtoks t1) _ _ _) = sort $ words t1

errortype :: Record -> [String]
errortype (Record _ _ t _ _) = shortenErrorType t

files = ["../../test/t1.xml","../../test/t2.xml"]
