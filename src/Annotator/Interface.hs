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
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Windows.Dialog
import Text.XML.HaXml.XmlContent.Haskell (readXml)

-- Helper function to associate all controls with actions
initControls :: GladeXML -> IO ()
initControls xml = do quitItem <- xmlGetWidget xml castToMenuItem menuItemQuit
                      openItem <- xmlGetWidget xml castToMenuItem menuItemOpen
                      window   <- xmlGetWidget xml castToWindow windowMain
                      quitItem `afterActivateLeaf` do widgetDestroy window
                      openItem `afterActivateLeaf` do fn <- openFileAction window
                                                      case fn of
                                                           (Just fn) -> loadFile xml fn
                                                           _         -> return ()
                      return ()

-- Queries the user for opening a file.
openFileAction :: Window -> IO (Maybe FilePath)
openFileAction window = do
    fc <- constructOpenFileChooser window
    r  <- dialogRun fc
    widgetHide fc
    case r of
         ResponseAccept -> fileChooserGetFilename fc
         _              -> return Nothing

-- Load a corpus from a file and display it.
loadFile :: GladeXML -> String -> IO ()
loadFile xml fn = do putStrLn $ "Opening File: " ++ fn
                     ta <- xmlGetWidget xml castToTextView corpusView
                     tb <- textViewGetBuffer ta
                     content <- readFile fn
                     let corpus = readXml content
                     case corpus of
                          Left  s -> showError s
                          Right s -> do tb `textBufferSetText` (xmlToTokenString s)

-- Takes a corpus and returns a string representing the corpus' text in plain text.
xmlToTokenString :: Corpus -> String
xmlToTokenString (Corpus (Tokens xs) _)= concat . map tokenToString $ xs
        where tokenToString (Token _ s) = s

-- Generic function to notify the user something bad has happened.
showError = undefined

-- Builds a GTK file Chooser to open files.
constructOpenFileChooser :: Window -> IO FileChooserDialog
constructOpenFileChooser w = do 
    fc <- fileChooserDialogNew (Just "Open Corpus")
                               (Just w)
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

-- Initialize the program's widget's events, listeners, and event handlers.
initWidgets :: GladeXML -> IO ()
initWidgets xml = do tv <- xmlGetWidget xml castToTextView corpusView
                     l  <- xmlGetWidget xml castToLabel "label1"
                     tv `on` buttonPressEvent $ findContext tv l
                     return ()

findContext :: TextView -> Label -> EventM EButton Bool
findContext tv l = do btn <- eventButton
                      case btn of
                           LeftButton -> do coords <- eventCoordinates
                                            let (x,y) = truncCoordToInt coords
                                            liftIO $ do bcrd <- tv `textViewWindowToBufferCoords` TextWindowWidget $ (x,y)
                                                        iter <- uncurry (textViewGetIterAtLocation tv) $ bcrd
                                                        iter' <- textIterCopy iter
                                                        textIterBackwardChars iter 5
                                                        textIterForwardChars iter' 5
                                                        slice <- textIterGetSlice iter iter'
                                                        l `labelSetText` slice
                                                        return False
                           _          -> return False

truncCoordToInt :: (Double,Double) -> (Int,Int)
truncCoordToInt (x,y) = (truncate x,truncate y)

-- | Entry in to the GUI
runGUI :: IO ()
runGUI = do prepareGUI
            mainGUI

-- | Entry in to the GUI, whilst opening a corpus file.
runGUIWithFile :: FilePath -> IO ()
runGUIWithFile fn = do xml <-  prepareGUI
                       loadFile xml fn
                       mainGUI

-- Common code used by GUI entry points, factored out. Returns the GladeXML of the main GUI.
prepareGUI :: IO GladeXML
prepareGUI = do initGUI
                Just xml <- xmlNew gladeSource
                window <- xmlGetWidget xml castToWindow windowMain
                initControls xml
                initWidgets xml
                onDestroy window mainQuit
                widgetShowAll window
                return xml
