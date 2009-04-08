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
        putStrLn "\nThree data points\n***"
        printColumn "io + iy + ir" $
                evaluate ((equality errtoks) /&\ (equality etype) /&\ (equality trigger)) es1 es2
        printColumn "so + iy + ir" $
                evaluate ((subset errtoks) /&\ (equality etype) /&\ (equality trigger)) es1 es2
        printColumn "io + sy + ir" $
                evaluate ((equality errtoks) /&\ (subtree etype) /&\ (equality trigger)) es1 es2
        printColumn "io + iy + sr" $
                evaluate ((equality errtoks) /&\ (equality etype) /&\ (subset trigger)) es1 es2
        printColumn "so + sy + ir" $
                evaluate ((subset errtoks) /&\ (subtree etype) /&\ (equality trigger)) es1 es2
        printColumn "io + sy + sr" $
                evaluate ((equality errtoks) /&\ (subtree etype) /&\ (subset trigger)) es1 es2
        printColumn "so + iy + sr" $
                evaluate ((subset errtoks) /&\ (equality etype) /&\ (subset trigger)) es1 es2
        printColumn "so + sy + sr" $
                evaluate ((subset errtoks) /&\ (subtree etype) /&\ (subset trigger)) es1 es2
        putStrLn "Two data points\n***"
        printColumn "io + iy" $
                evaluate ((equality errtoks) /&\ (equality etype)) es1 es2
        printColumn "so + iy" $
                evaluate ((subset errtoks) /&\ (equality etype)) es1 es2
        printColumn "io + sy" $
                evaluate ((equality errtoks) /&\ (subtree etype)) es1 es2
        printColumn "so + sy" $
                evaluate ((subset errtoks) /&\ (subtree etype)) es1 es2
        printColumn "io + ir" $
                evaluate ((equality errtoks) /&\ (equality trigger)) es1 es2
        printColumn "so + ir" $
                evaluate ((subset errtoks) /&\ (equality trigger)) es1 es2
        printColumn "io + sr" $
                evaluate ((equality errtoks) /&\ (subset trigger)) es1 es2
        printColumn "so + sr" $
                evaluate ((subset errtoks) /&\ (subset trigger)) es1 es2
        putStrLn "\nOne Data point\n***"
        printColumn "io" $
                evaluate ((equality errtoks)) es1 es2
        printColumn "so" $
                evaluate ((subset errtoks)) es1 es2

(\||/) :: [String] -> [String] -> Bool
[] \||/ _              = True
_ \||/ []              = True
(sl:ssl) \||/ (sr:ssr) =  sl == sr &&  ssl \||/ ssr

subset f l r | (f l) == [] || (f r) == [] = True
subset f l r = intersect (f l) (f r) /= []
equality f l r = (f l) == (f r)
subtree f l r = f l \||/ f r


(/&\) :: (a -> b -> Bool) -> (a -> b -> Bool) -> (a -> b -> Bool)
f /&\ s = \l r -> f l r && s l r

evaluate f = \l r -> show . length $ intersectBy f l r


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

etype :: Record -> [String]
etype (Record _ _ t _ _) = shortenErrorType t

trigger :: Record -> [String]
trigger (Record (Record_Attrs Nothing _ ) _ _ _ _) = []
trigger (Record (Record_Attrs (Just trigger) _ ) _ _ _ _) = sort $ words trigger

files = ["../../test/t1.xml","../../test/t2.xml"]
