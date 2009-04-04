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
import Annotator.Interface.Models
import Annotator.Interface.Handlers
import Annotator.Interface.Util
import Annotator.Interface.Types
import Control.Monad.Trans (liftIO)
import Control.Monad (forM,zipWithM_)
import Data.IORef
import Data.List
import qualified Data.Map as M
import GHC.List hiding (span)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import qualified Graphics.UI.Gtk.Gdk.Events as Old
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Windows.Dialog
import Text.XML.HaXml.XmlContent.Haskell (readXml)
import Text.XML.HaXml.XmlContent (XmlContent, fWriteXml)

import Data.Array

token2Tag :: Gui -> TextTagTable -> Token -> IO TextTag
token2Tag gui tt token@(Token (Token_Attrs idx) _) = do tag <- textTagNew $ Just idx
                                                        tag `onTextTagEvent` (tagEventHandler gui token)
                                                        tt `textTagTableAdd` tag
                                                        return tag

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

-- | Entry to the GUI
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
                             nothingRef' <- newIORef Nothing
                             nothingRef'' <- newIORef []
                             nothingRef''' <- newIORef Nothing
                             nothingRef'''' <- newIORef []
                             let gui = Gui { corpusView  = textView
                                           , window      = w
                                           , xml         = gladeXml
                                           , tokenLabel  = tl
                                           , tokenArray  = nothingRef'
                                           , selectedTkn = nothingRef''
                                           , xmlDocument = nothingRef'''
                                           , trigger     = nothingRef''''
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
                      openItem `afterActivateLeaf` openItemHandler gui
                      clearBtn `onClicked` clearBtnHandler gui
                      errorView <- xmlGetWidget (xml gui) castToTreeView "errorView"
                      (initTreeView errorView) =<< errorStore
                      recordBtn <- xmlGetWidget (xml gui) castToButton "recordButton"
                      recordBtn `onClicked` recordHandler gui errorView
                      return ()

initTreeView :: TreeView -> TreeStore EType -> IO ()
initTreeView view model =  do
                        view `treeViewSetModel` model
                        view `treeViewSetHeadersVisible` False
                        column <- treeViewColumnNew
                        renderer <- cellRendererTextNew
                        cellLayoutPackStart column renderer True
                        cellLayoutSetAttributes column renderer model $ \row -> [cellText := name row]
                        view `treeViewAppendColumn` column
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

openItemHandler :: Gui -> IO ()
openItemHandler gui = do fn <- openFileAction gui
                         case fn of
                              (Just fn') -> loadFile gui fn'
                              _          -> return ()

saveItemHandler :: Gui -> FilePath -> IO ()
saveItemHandler gui fn = do ref <- readIORef (xmlDocument gui)
                            case ref of
                                 Just doc -> fWriteXml fn doc
                                 Nothing  -> showError "Load a file first"

saveAsItemHandler :: Gui -> IO ()
saveAsItemHandler = undefined

-- Load a corpus from a file, display it, and set the appropriate events.
loadFile :: Gui -> FilePath -> IO ()
loadFile gui fn = do
    cntt <- readFile fn
    let corpus = readXml cntt
    case corpus of
         Left  s -> showError $ "XML Parsing failed: " ++ s
         Right c -> do
               updateRef (xmlDocument gui) c
               si <- xmlGetWidget (xml gui) castToMenuItem "menuItemSave"
               si `afterActivateLeaf` (saveItemHandler gui fn)
               widgetSetSensitive si True

               tb <- textViewGetBuffer (corpusView gui)
               tagtable <- textBufferGetTagTable tb
               readCorpus c tb gui

readCorpus :: Corpus -> TextBuffer -> Gui -> IO ()
readCorpus corpus@(Corpus (Tokens _ ts) (Errors es)) tb gui =
        do putStrLn "Indexing tokens..."
           updateRef (tokenArray gui) tokens
           putStrLn "Filling Buffer..."
           tb `textBufferSetText` (concat . (map tokenString) $ tokenList)
           tt <- textBufferGetTagTable tb
           putStrLn "Creating tags..."
           tags <- forM tokenList (token2Tag gui tt)
           putStrLn "Applying tags..."
           applyTags tb tags tokenList
           putStrLn "Finished."
           where tokens = xmlToArray corpus
                 tokenList = elems tokens


applyTags :: TextBuffer -> [TextTag] -> [Token] -> IO ()
applyTags tb tags tokens = do tt <- textBufferGetTagTable tb
                              iter <- textBufferGetStartIter tb
                              applyTag tags tokens iter
                              where applyTag :: [TextTag] -> [Token] -> TextIter -> IO ()
                                    applyTag (g:gs) ((Token _ t):ts) iter = do 
                                             iter' <- textIterCopy iter
                                             textIterForwardChars iter' (length t)
                                             textBufferApplyTag tb g iter iter'
                                             applyTag gs ts iter'
                                    applyTag [] [] _ = return ()
                                    applyTag _ _ _ = error "Aleks fucked up." -- this shouldn't happen

