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

              files@(_:_:_) -> do parses <- forM files (\f -> fmap readXml (readFile f))
                                  case lefts parses of
                                       [] -> compareCorpora $ rights parses
                                       errors -> do forM_ errors putStrLn
              _       -> putStrLn "Usage: agreement a1.xml a2.xml"

compareCorpora :: [Corpus] -> IO ()
compareCorpora ((Corpus (Tokens a1 ts1) (Errors es1)):(Corpus (Tokens a2 ts2) (Errors es2)):[]) = do
    putStrLn "Testing for token to token Consistency..."
    if a1 /= a2 || ts1 /= ts2
       then putStrLn "Tokens inconsistent. Checking agreement doesn't make sense this way."
       else do putStrLn "Tokens consistent."
               let results = compareErrors2 es1 es2
               undefined
compareCorpora _          = putStrLn "Sorry, but only two corpora can be evaluated at this point."

compareErrors2 :: [Record] -> [Record] -> ComparedRecords
compareErrors2 es1 es2 = let ses1 = sortBy tokenEquality es1
                             ses2 = sortBy tokenEquality es2
                         in trawlErrors ses1 ses2 (ComparedRecords [] ([],[]))

trawlErrors :: [Record] -> [Record] -> ComparedRecords -> ComparedRecords
trawlErrors [] _ c = c
trawlErrors _ [] c = c
trawlErrors (r1@(Record _ (Errtoks t1) _ _ _):rs1)
            (r2@(Record _ (Errtoks t2) _ _ _):rs2)
            (ComparedRecords agree disagree@(lrds,rrds))
            | t1 == t2  = trawlErrors rs1 rs2
                                      (ComparedRecords ((RecordPair r1 r2):agree) disagree)
            | otherwise = trawlErrors rs1 rs2
                                      (ComparedRecords agree ((r1:lrds),(r2:rrds)))

tokenEquality :: Record -> Record -> Ordering
tokenEquality (Record _ (Errtoks t1) _ _ _) (Record _ (Errtoks t2) _ _ _) = compare t1 t2
