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

instance Ord Token where
    compare (Token (Token_Attrs t1) _) (Token (Token_Attrs t2) _) = compare (f t1) (f t2)
                    where f :: String -> Int
                          f = read . (drop 1)
