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

xmlToArray :: Corpus -> Array Int Token
xmlToArray (Corpus (Tokens (Tokens_Attrs amount) ts) _) =
        array (0,amount') [(i,t) | (i,t) <-map f ts]
        where amount' = (read amount) - 1
              f t@(Token (Token_Attrs idx) _) = (read . (drop 1) $ idx,t)

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

truncCoordToInt :: (Double,Double) -> (Int,Int)
truncCoordToInt (x,y) = (truncate x,truncate y)

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
                             nothingRef <- newIORef Nothing
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
                      spellBtn <- xmlGetWidget (xml gui) castToButton "errorSpelButton"
                      triggBtn <- xmlGetWidget (xml gui) castToToggleButton "triggerButton"
                      quitItem `afterActivateLeaf` widgetDestroy (window gui)
                      openItem `afterActivateLeaf` openItemHandler gui
                      clearBtn `onClicked` clearBtnHandler gui
                      spellBtn `onClicked` spellBtnHandler gui
                      ref <- newIORef Nothing
                      grmview  <- xmlGetWidget (xml gui) castToTreeView "grammarView"
                      frmview  <- xmlGetWidget (xml gui) castToTreeView "formView"
                      (initTreeView grmview) =<< grammarStore
                      (initTreeView frmview) =<< formStore
                      frecBtn <- xmlGetWidget (xml gui) castToButton "formRecordButton"
                      grecBtn <- xmlGetWidget (xml gui) castToButton "grammarRecordButton"
                      frecBtn `onClicked` frecBtnHandler gui frmview
                      grecBtn `onClicked` grecBtnHandler gui grmview

                      return ()

grecBtnHandler :: Gui -> TreeView -> IO ()
grecBtnHandler gui view = undefined

initTreeView :: (XmlContent a) => TreeView -> TreeStore (EType a) -> IO ()
initTreeView view model =  do
                        view `treeViewSetModel` model
                        view `treeViewSetHeadersVisible` False
                        column <- treeViewColumnNew
                        renderer <- cellRendererTextNew
                        cellLayoutPackStart column renderer True
                        cellLayoutSetAttributes column renderer model $ \row -> [cellText := name row]
                        view `treeViewAppendColumn` column
                        return ()

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

spellBtnHandler :: Gui -> IO ()
spellBtnHandler gui = do tks <- readIORef (selectedTkn gui)
                         case tks of
                              [] -> showError "Please select some tokens first!"
                              ts -> do addToErrors gui e
                                       clearBtnHandler gui
                                       where e = (Error (Errtoks $ unwords . (map tokenId) $ ts)
                                                        (TypeSpelling Spelling)
                                                        Nothing
                                                        Nothing)
tokenId :: Token -> String
tokenId (Token (Token_Attrs idx) _) = idx

frecBtnHandler :: Gui -> TreeView -> IO ()
frecBtnHandler gui view = do row <- (treeViewGetSelection view >>= treeSelectionGetSelectedRows)
                             tks <- readIORef (selectedTkn gui)
                             case tks of
                                  [] -> showError "Please select some tokens first"
                                  ts -> case row of
                                             [path] -> do (EType _ content) <- (\s -> s `treeStoreGetValue` path) =<< formStore
                                                          case content of
                                                               Nothing -> showError "Select a leaf."
                                                               Just etype -> do addToErrors gui e
                                                                                clearBtnHandler gui
                                                                                where e = (Error (Errtoks $ unwords . (map tokenId) $ ts)
                                                                                                 (TypeForm etype)
                                                                                                  Nothing
                                                                                                  Nothing)
                                             _ -> showError "Please select one item."

clearBtnHandler :: Gui -> IO ()
clearBtnHandler gui =  do writeIORef (selectedTkn gui) []
                          putTokensOnLabel gui (tokenLabel gui)

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

token2Tag :: Gui -> TextTagTable -> Token -> IO TextTag
token2Tag gui tt token@(Token (Token_Attrs idx) _) = do tag <- textTagNew $ Just idx
                                                        tag `onTextTagEvent` (tagEventHandler gui token)
                                                        tt `textTagTableAdd` tag
                                                        return tag

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

tagEventHandler :: Gui -> Token -> Old.Event -> TextIter -> IO ()
tagEventHandler gui t (Old.Button _ Old.SingleClick _ _ _ _ Old.LeftButton  _ _) _ = do
    seltokens <- readIORef (selectedTkn gui)
    if t `elem` seltokens
       then writeIORef (selectedTkn gui) (t `delete` seltokens)
       else writeIORef (selectedTkn gui) (t:seltokens)
    putTokensOnLabel gui (tokenLabel gui)
tagEventHandler _ _ _ _ = return ()

putTokensOnLabel :: Gui -> Label -> IO ()
putTokensOnLabel gui l = do tkns <- readIORef (selectedTkn gui)
                            l `labelSetText` (show $ map tokenString (sort tkns))

-- Helper for maps. Factored out due to common use.
tokenString :: Token -> String
tokenString (Token _ s) = s

updateRef  :: IORef (Maybe a) -> a -> IO ()
updateRef ref payload = updateRef' ref (const $ return payload)

updateRef' :: IORef (Maybe a) -> (Maybe a -> IO a) -> IO ()
updateRef' ref act = do var <- readIORef ref
                        a'  <- act var
                        writeIORef ref (Just a')

addToErrors :: Gui -> Error -> IO ()
addToErrors gui err = do ref <- readIORef (xmlDocument gui)
                         case ref of
                              Just doc -> writeIORef (xmlDocument gui) (Just $ (ate err) doc)
                              Nothing  -> return ()
                              where  ate e crp@(Corpus ts (Errors es)) =
                                            if e `elem` es
                                               then crp
                                               else Corpus ts (Errors (e:es))

removeFromErrors :: Gui -> Error -> IO ()
removeFromErrors gui err = do ref <- readIORef (xmlDocument gui)
                              case ref of
                                   Just doc -> writeIORef (xmlDocument gui) (Just $ (rfe err) doc)
                                   Nothing  -> return ()
                                   where  rfe e (Corpus ts (Errors es)) =
                                                 Corpus ts (Errors (delete e es))
