-- |
-- Module : Annotator
-- Copyright : 2009 Aleksandar Dimitrov
-- License : BSD3
--
-- Maintainer : Aleksandar Dimitrov <aleks.dimitrov@gmail.com>
-- Stability : provisional
-- Portability : unportable
-- 
-- This module defines the graphical user interface for the Annotator.

module Annotator.Interface
       ( -- * GUI entry points  
       runGUI
       , runGUIWithFile 
       ) where

import Annotator.DTD
import Annotator.Interface.Constants
import Control.Monad.Trans (liftIO)
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Windows.Dialog
import Text.XML.HaXml.XmlContent.Haskell (readXml)

-- | GUI state
data Gui = Gui { corpusView  :: TV -- ^ The corpusView
               , window      :: Window -- ^ The main window
               , xml         :: GladeXML -- ^ The underlying glade XML
               , corpusClick :: Maybe (ConnectId TextView) -- ^ The onClick event on corpusView
               }

data TV = TV { tv :: TextView
             , i :: IORef (Maybe (ConnectId TextView))
             }            

-- Takes a corpus and returns a string representing the corpus' text in plain text.
xmlToTokenString :: Corpus -> String
xmlToTokenString (Corpus (Tokens xs) _)= concatMap tokenToString xs
        where tokenToString (Token _ s) = s

-- Generic function to notify the user something bad has happened.
showError = undefined

-- Builds a GTK file Chooser to open files.
constructOpenFileChooser :: Gui -> IO FileChooserDialog
constructOpenFileChooser gui = do
    fc <- fileChooserDialogNew (Just "Open Corpus")
                               (Just (window gui))
                               FileChooserActionOpen
                               [("gtk-cancel",ResponseCancel),("gtk-open",ResponseAccept)]
    fileChooserSetSelectMultiple fc False
    ff <- xmlFileFilter
    fileChooserAddFilter fc ff
    return fc

-- Builds a GTK file filter to only allow XML documents.
xmlFileFilter :: IO FileFilter
xmlFileFilter = do ff <- fileFilterNew
                   fileFilterSetName ff "XML files"
                   fileFilterAddMimeType ff "text/xml"
                   return ff

findContext :: Gui -> Label -> EventM EButton Bool
findContext gui l = do btn <- eventButton
                       case btn of
                            LeftButton -> do coords <- eventCoordinates
                                             let (x,y) = truncCoordToInt coords
                                             liftIO $ do bcrd <- tv (corpusView gui) `textViewWindowToBufferCoords` TextWindowWidget $ (x,y)
                                                         iter <- uncurry (textViewGetIterAtLocation (tv(corpusView gui))) bcrd
                                                         iter' <- textIterCopy iter
                                                         textIterBackwardChars iter 5
                                                         textIterForwardChars iter' 5
                                                         slice <- textIterGetSlice iter iter'
                                                         l `labelSetText` slice
                                                         putStrLn slice
                                                         return False
                            _          -> return False

truncCoordToInt :: (Double,Double) -> (Int,Int)
truncCoordToInt (x,y) = (truncate x,truncate y)

-- | Entry in to the GUI
runGUI :: IO ()
runGUI = prepareGUI >> mainGUI

-- | Entry in to the GUI, whilst opening a corpus file.
runGUIWithFile :: FilePath -> IO ()
runGUIWithFile fn = do gui <-  prepareGUI
                       loadFile gui fn
                       mainGUI

-- Common code used by GUI entry points, factored out. Returns the GladeXML of the main GUI.
prepareGUI :: IO Gui
prepareGUI = do
    initGUI
    mXml <- xmlNew gladeSource
    case mXml of
         Nothing       -> showError $ "Invalid glade xml file " ++ gladeSource
         Just gladeXml -> do w      <- xmlGetWidget gladeXml castToWindow windowMain
                             textView <- xmlGetWidget gladeXml castToTextView "corpusView"
                             nothingRef <- newIORef Nothing
                             let gui = Gui { corpusView  = TV textView nothingRef
                                           , corpusClick = Nothing
                                           , window      = w
                                           , xml         = gladeXml
                                           }
                             initControls gui
                             onDestroy w mainQuit
                             widgetShowAll w
                             return gui

-- Helper function to associate all controls with actions
initControls :: Gui -> IO ()
initControls gui = do quitItem <- xmlGetWidget (xml gui) castToMenuItem menuItemQuit
                      openItem <- xmlGetWidget (xml gui) castToMenuItem menuItemOpen
                      quitItem `afterActivateLeaf` widgetDestroy (window gui)
                      openItem `afterActivateLeaf` do fn <- openFileAction gui
                                                      case fn of
                                                           (Just fn') -> loadFile gui fn'
                                                           _          -> return ()
                      return ()

-- Queries the user for opening a file.
openFileAction :: Gui -> IO (Maybe FilePath)
openFileAction gui = do
    fc <- constructOpenFileChooser gui
    r  <- dialogRun fc
    widgetHide fc
    case r of
         ResponseAccept -> fileChooserGetFilename fc
         _              -> return Nothing

-- Load a corpus from a file, display it, and set the appropriate events.
loadFile :: Gui -> String -> IO ()
loadFile gui fn = do l  <- xmlGetWidget (xml gui) castToLabel "label1"
                     tb <- textViewGetBuffer (tv (corpusView gui))
                     content <- readFile fn
                     let corpus = readXml content
                     case corpus of
                          Left  s -> showError $ "XML Parsing failed. " ++ s
                          Right s -> tb `textBufferSetText` xmlToTokenString s
                     ref <- readIORef (i (corpusView gui))
                     case ref of
                          (Just i') -> signalDisconnect i'
                          Nothing   -> return ()
                     connectId <- tv (corpusView gui) `on` buttonPressEvent $ findContext gui l
                     writeIORef (i (corpusView gui)) $ Just connectId
                     return ()

