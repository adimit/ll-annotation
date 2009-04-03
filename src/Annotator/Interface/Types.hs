-- | A small module to contain all relevant data types for the Interface.

module Annotator.Interface.Types where

import Annotator.DTD
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Annotator.DTD

import Data.Array

-- | GUI state
data Gui = Gui { corpusView  :: TextView -- ^ The corpusView
               , window      :: Window -- ^ The main window
               , xml         :: GladeXML -- ^ The underlying glade XML
               , tokenLabel  :: Label -- ^ The Label displaying tokens
               , selectedTkn :: IORef [Token]
               , trigger     :: IORef [Token]
               , tokenArray  :: IORef (Maybe (Array Int Token))
               , xmlDocument :: IORef (Maybe Corpus)
               }

type Id = String

type TokenMap = Map Id (Token,(Int,Int))

data Span = Span Int Int
          | Point Int 
          deriving (Show)

instance Eq Span where
    x /= y = not $ x == y
    (Span x1 s1) == (Span x2 s2) | x1 == x2 && s1 == s2 = True
                                 | otherwise            = False
    (Point p1)   == (Point p2)   | p1 == p2             = True
                                 | otherwise            = False
    (Span x s)   == (Point p)    | p >= x && p < (x+s)  = True
                                 | otherwise            = False
    p@(Point _)  == s@(Span _ _) = s == p
    
instance Ord Span where
    compare (Span x1 s1) (Span x2 s2) | x1 < x2 = LT
                                      | x2 < x1 = GT
                                      | x1 == x2 = compare s1 s2
                                      | otherwise = error "This shouldn't happen."
    compare (Span x s) (Point p) | p < x     = LT
                                 | p > (x+s) = GT
                                 | otherwise = EQ
    compare p@(Point _) s@(Span _ _) = compare s p
    compare (Point p1) (Point p2) = compare p1 p2

instance Ord Token where
    compare (Token (Token_Attrs t1) _) (Token (Token_Attrs t2) _) = compare (f t1) (f t2)
                    where f :: String -> Int
                          f = read . (drop 1)
    
newtype Index = Index String deriving (Eq, Show)

instance Ord Index where
    compare (Index (_:t1)) (Index (_:t2)) = compare (read t1 :: Int) (read t2 :: Int)
    compare a b = error $ "Comparision of " ++ (show a) ++ " and " ++ (show b) ++ " not possible"

