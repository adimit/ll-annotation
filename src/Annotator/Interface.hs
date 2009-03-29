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
import Annotator.Interface.Types
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import GHC.List hiding (span)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Windows.Dialog
import Text.XML.HaXml.XmlContent.Haskell (readXml)

-- | GUI state
data Gui = Gui { corpusView  :: TextView -- ^ The corpusView
               , window      :: Window -- ^ The main window
               , xml         :: GladeXML -- ^ The underlying glade XML
               , tokenLabel  :: Label -- ^ The Label displaying tokens
               , corpusClick :: IORef (Maybe (ConnectId TextView))
               , tokens      :: IORef (Maybe TokenMap)
               , selectedTkn :: IORef (Maybe [Token])
               , currentErrs :: IORef (Maybe [Error])
               }

-- Takes a corpus and returns a string representing the corpus' text in plain text.
xmlToTokenString :: Corpus -> (Int,String,TokenMap)
xmlToTokenString (Corpus (Tokens xs) _) = foldl' f (0,"",M.empty) xs
        where f !(!i,!s,!m) !token@(Token _ t) = 
               let l = length t
               in  (i+l,s++t,M.insert (Span i l) token m)

-- Generic function to notify the user something bad has happened.
showError :: String -> IO ()
showError = putStrLn

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

findContext :: Gui -> EventM EButton Bool
findContext gui =
    do btn <- eventButton
       case btn of
           LeftButton -> do 
               coords <- eventCoordinates
               let (x,y) = truncCoordToInt coords
               liftIO $ do findToken gui (x,y)
                           return False
           _          -> return False
       
truncCoordToInt :: (Double,Double) -> (Int,Int)
truncCoordToInt (x,y) = (truncate x,truncate y)

findToken :: Gui -> (Int,Int) -> IO ()
findToken gui (x,y) = do
        bcrd <- (corpusView gui) `textViewWindowToBufferCoords` TextWindowWidget $ (x,y)
        iter <- uncurry (textViewGetIterAtLocation (corpusView gui)) bcrd
        loc <- textIterGetOffset iter
        Just tmap <- readIORef (tokens gui)
        let Just token = (Point loc) `M.lookup` tmap
        ref <- readIORef (selectedTkn gui)
        case ref of
             Nothing -> writeIORef (selectedTkn gui) (Just [token])
             Just toks -> if (not (elem token toks))
                               then writeIORef (selectedTkn gui) (Just $ toks ++ [token])
                               else writeIORef (selectedTkn gui) (Just $ delete token toks)
        putTokensOnLabel gui
                        
                                      
-- | Entry in to the GUI
runGUI :: IO ()
runGUI = do gui <- prepareGUI 
            case gui of
                 Left s -> showError s
                 Right _ -> mainGUI

-- | Entry in to the GUI, whilst opening a corpus file.
runGUIWithFile :: FilePath -> IO ()
runGUIWithFile fn = do gui <-  prepareGUI
                       case gui of 
                            Left s -> showError s
                            Right g -> loadFile g fn >> mainGUI

-- Common code used by GUI entry points. Returns the GladeXML of the main GUI.
prepareGUI :: IO (Either String Gui)
prepareGUI = do
    initGUI
    mXml <- xmlNew gladeSource
    case mXml of
         Nothing       -> return $ Left ("Invalid glade xml file " ++ gladeSource)
         Just gladeXml -> do -- beware of ugliness ahead. This is a FIXME
                             w       <- xmlGetWidget gladeXml castToWindow windowMain
                             tl       <- xmlGetWidget gladeXml castToLabel "tokenLabel"
                             textView  <- xmlGetWidget gladeXml castToTextView "corpusView"
                             nothingRef <- newIORef Nothing
                             nothingRef' <- newIORef Nothing
                             nothingRef'' <- newIORef Nothing
                             nothingRef''' <- newIORef Nothing
                             let gui = Gui { corpusView  = textView
                                           , window      = w
                                           , xml         = gladeXml
                                           , tokenLabel  = tl
                                           , corpusClick = nothingRef
                                           , tokens      = nothingRef'
                                           , selectedTkn = nothingRef''
                                           , currentErrs = nothingRef'''
                                           }
                             initControls gui
                             onDestroy w mainQuit
                             widgetShowAll w
                             return $ Right gui

-- Helper function to associate all controls with actions
initControls :: Gui -> IO ()
initControls gui = do quitItem <- xmlGetWidget (xml gui) castToMenuItem menuItemQuit
                      openItem <- xmlGetWidget (xml gui) castToMenuItem menuItemOpen
                      clearBtn <- xmlGetWidget (xml gui) castToButton "clearButton"
                      quitItem `afterActivateLeaf` widgetDestroy (window gui)
                      openItem `afterActivateLeaf` do fn <- openFileAction gui
                                                      case fn of
                                                           (Just fn') -> loadFile gui fn'
                                                           _          -> return ()
                      
                      clearBtn `onClicked` do writeIORef (selectedTkn gui) (Just [])
                                              putTokensOnLabel gui
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
loadFile gui fn = do
    tb <- textViewGetBuffer (corpusView gui)
    content <- readFile fn
    let corpus = readXml content
    case corpus of
         Left  s -> showError $ "XML Parsing failed. " ++ s
         Right c -> do
               let (_,text,toks) = xmlToTokenString c
               tb `textBufferSetText` text
               connectId <- corpusView gui `on` buttonPressEvent $ findContext gui
               updateRef (corpusClick gui) connectId (\(s::ConnectId TextView) -> signalDisconnect s)
               updateRef (tokens gui) toks (const $ return ())
               return ()
    
putTokensOnLabel :: Gui -> IO ()
putTokensOnLabel gui = do ref <- readIORef (selectedTkn gui)
                          case ref of
                               (Just tkns) -> (tokenLabel gui) `labelSetText` (show (map render tkns))
                               Nothing     -> (tokenLabel gui) `labelSetText` ""

updateRef :: IORef (Maybe a) -> a -> (a -> IO ()) -> IO ()
updateRef ref new f = do var <- readIORef ref
                         case var of
                              Just d -> f d
                              Nothing -> return ()
                         writeIORef ref $ Just new
                         return ()

render :: Token -> String
render (Token _ s) = s