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
import Control.Monad (forM,forM_)
import Data.IORef
import GHC.List hiding (span)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Windows.Dialog
import Text.XML.HaXml.XmlContent.Haskell (readXml)
import Text.XML.HaXml.XmlContent (fWriteXml)

import Data.Array

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
                             trigButton  <- xmlGetWidget gladeXml castToToggleButton "triggerButton"
                             nothingRef' <- newIORef (array (0,0) [])
                             nothingRef'' <- newIORef []
                             nothingRef''' <- newIORef Nothing
                             nothingRef'''' <- newIORef []
                             listRef <- newIORef =<< (listStoreNew [])
                             somethingRef <- newIORef nothingRef''
                             let gui = Gui { corpusView  = textView
                                           , window      = w
                                           , xml         = gladeXml
                                           , tokenLabel  = tl
                                           , triggerBtn  = trigButton
                                           , tokenArray  = nothingRef'
                                           , selectedTkn = nothingRef''
                                           , xmlDocument = nothingRef'''
                                           , errorModel  = listRef
                                           , currentFocs = somethingRef
                                           , trigger     = nothingRef''''
                                           }
                             initControls gui
                             onDestroy w mainQuit
                             widgetShowAll w
                             return $ Right gui


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

-- Helper function to associate all controls with actions
initControls :: Gui -> IO ()
initControls gui = do quitItem <- xmlGetWidget (xml gui) castToMenuItem menuItemQuit
                      openItem <- xmlGetWidget (xml gui) castToMenuItem menuItemOpen
                      clearBtn <- xmlGetWidget (xml gui) castToButton "clearButton"
                      quitItem `afterActivateLeaf` widgetDestroy (window gui)
                      openItem `afterActivateLeaf` openItemHandler gui
                      clearBtn `onClicked` clearBtnHandler gui
                      deleteBtn <- xmlGetWidget (xml gui) castToButton "deleteButton"
                      errorView <- xmlGetWidget (xml gui) castToTreeView "errorView"
                      listView <- xmlGetWidget (xml gui) castToTreeView "treeView1"
                      (initTreeView errorView) =<< errorStore
                      (initListView gui listView) =<< (readIORef (errorModel gui))
                      recordBtn <- xmlGetWidget (xml gui) castToButton "recordButton"
                      deleteBtn `onClicked` deleteHandler gui listView
                      recordBtn `onClicked` recordHandler gui errorView
                      (triggerBtn gui) `afterToggled` triggerToggleHandler gui
                      return ()

initTreeView :: TreeView -> TreeStore (EType Error) -> IO ()
initTreeView view model =  do
                        view `treeViewSetModel` model
                        view `treeViewSetHeadersVisible` False
                        column <- treeViewColumnNew
                        renderer <- cellRendererTextNew
                        cellLayoutPackStart column renderer True
                        cellLayoutSetAttributes column renderer model $ \row -> [cellText := name row]
                        view `treeViewAppendColumn` column
                        return ()

initListView :: Gui -> TreeView -> ListStore Record -> IO ()
initListView gui view model = do
    view `treeViewSetModel` model
    view `treeViewSetHeadersVisible` True
    
    tokens <- readIORef (tokenArray gui)
    
    columnTokens <- treeViewColumnNew
    rendererTokens <- cellRendererTextNew
    treeViewColumnPackStart columnTokens rendererTokens True
    cellLayoutSetAttributes columnTokens rendererTokens model $ \row -> [cellText := record2tokens row tokens ]
    treeViewColumnSetTitle columnTokens "Error Tokens"
    view `treeViewAppendColumn` columnTokens
    
    columnTrigger <- treeViewColumnNew
    rendererTrigger <- cellRendererTextNew
    treeViewColumnPackStart columnTrigger rendererTrigger True
    cellLayoutSetAttributes columnTrigger rendererTrigger model $ \row -> [cellText := record2trigger row tokens ]
    treeViewColumnSetTitle columnTrigger "Trigger"
    view `treeViewAppendColumn` columnTrigger
    
    columnType <- treeViewColumnNew
    rendererType <- cellRendererTextNew
    treeViewColumnPackStart columnType rendererType True
    cellLayoutSetAttributes columnType rendererType model $ \row -> [cellText := record2type row ]
    treeViewColumnSetTitle columnType "Type"
    view `treeViewAppendColumn` columnType
    
    return ()

record2tokens (Record _ (Errtoks errtoks) _ _ _) tokens = show $ (map (read . drop 1) (words errtoks)::[Int])

record2trigger (Record (Record_Attrs Nothing _) _ _ _ _) tokens = "N/A"
record2trigger (Record (Record_Attrs (Just context) _) _ _ _ _) tokens = context
record2type (Record _ _ etype _ _) = show etype

lookupIndices array = map (array!)
                                
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
saveItemHandler gui fn = do errors <- listStoreToList =<< (readIORef $ errorModel gui)
                            ref <- readIORef (xmlDocument gui)
                            case ref of
                                 Just (Corpus ts _) -> fWriteXml fn (Corpus ts (Errors errors))
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
               readCorpus c tb gui

readCorpus :: Corpus -> TextBuffer -> Gui -> IO ()
readCorpus corpus@(Corpus (Tokens _ _) (Errors es)) tb gui =
        do putStrLn "Indexing tokens..."
           writeIORef (tokenArray gui) tokens
           putStrLn "Filling Buffer..."
           tb `textBufferSetText` (concat . (map tokenString) $ tokenList)
           tt <- textBufferGetTagTable tb
           putStrLn "Creating tags..."
           tags <- forM tokenList (token2Tag gui tt)
           putStrLn "Applying tags..."
           applyTags tb tags tokenList
           putStrLn "Reading in records..."
           forM_ es (addToErrorView gui)
           putStrLn "Finished."
           -- putStrLn $ show $ assocs tokens
           where tokens = xmlToArray corpus
                 tokenList = elems tokens


applyTags :: TextBuffer -> [TextTag] -> [Token] -> IO ()
applyTags tb tags tokens = do iter <- textBufferGetStartIter tb
                              applyTag tags tokens iter
                              where applyTag :: [TextTag] -> [Token] -> TextIter -> IO ()
                                    applyTag (g:gs) ((Token _ t):ts) iter = do 
                                             iter' <- textIterCopy iter
                                             textIterForwardChars iter' (length t)
                                             textBufferApplyTag tb g iter iter'
                                             applyTag gs ts iter'
                                    applyTag [] [] _ = return ()
                                    applyTag _ _ _ = error "Aleks fucked up." -- this shouldn't happen

token2Tag :: Gui -> TextTagTable -> Token -> IO TextTag
token2Tag gui tt token@(Token (Token_Attrs idx) _) = do tag <- textTagNew $ Just idx
                                                        tag `onTextTagEvent` (tagEventHandler gui token)
                                                        tt `textTagTableAdd` tag
                                                        return tag

