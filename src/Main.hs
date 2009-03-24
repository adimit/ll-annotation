module Main where
    
import Annotator.Interface
import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          case args of 
               [] -> runGUI
               [fn] -> runGUIWithFile fn
               _    -> putStrLn "Please provide either exactly one file name or no arguments."
