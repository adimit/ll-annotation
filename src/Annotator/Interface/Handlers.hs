module Annotator.Interface.Handlers where

import Annotator.Interface.Types
import Annotator.Interface.Util
import Annotator.Interface.Models
import Annotator.DTD
import Control.Monad (forM_)
import Data.IORef
import Data.List
import GHC.List hiding (span)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import qualified Graphics.UI.Gtk.Gdk.Events as Old
import System.Glib.Attributes
import Text.XML.HaXml.XmlContent (XmlContent)

--changeButtonHandler :: Gui -> IO ()
changeButtonHandler gui etypeview elist = do
    paths <- treeSelectionGetSelectedRows =<< (treeViewGetSelection etypeview)
    lmodel <- readIORef (errorModel gui)
    rows <- treeSelectionGetSelectedRows =<< (treeViewGetSelection elist)

    case paths of
         [path] -> case rows of
                        [row] -> do (EType _ record) <- (\store -> store `treeStoreGetValue` path) =<< errorStore
                                    (Record attrs tokens _ target comment) <- lmodel `listStoreGetValue` (head row)
                                    deleteThisRecord gui row
                                    lmodel `listStoreAppend` (Record attrs tokens record target comment)
                                    return ()
                        _ -> putStrLn "No row seleceted"
         _ -> putStrLn "No path seleceted"



deleteHandler :: Gui -> TreeView -> IO ()
deleteHandler gui view = do
    rows <- treeSelectionGetSelectedRows =<< (treeViewGetSelection view)
    forM_ rows (deleteThisRecord gui)

deleteThisRecord :: Gui -> TreePath -> IO ()
deleteThisRecord gui [index] = do model <- readIORef (errorModel gui)
                                  model `listStoreRemove` index
deleteThisRecord _ _ = putStrLn "Panic! We got a tree path and wanted a list index"

recordHandler :: Gui -> TreeView -> IO ()
recordHandler gui view = 
    do tokens <- readIORef (selectedTkn gui)
       case tokens of
         [] -> putStrLn "Please select some tokens."
         _ -> do paths <- treeSelectionGetSelectedRows =<< (treeViewGetSelection view)
                 case paths of
                   [path] -> do (EType _ etype) <- (\store ->treeStoreGetValue store path) =<< errorStore
                                record <- makeRecord gui etype
                                addToErrorView gui record
                                clearBtnHandler gui
                   _ -> putStrLn "Warning: Select one error type first."

addToErrorView :: Gui -> Record -> IO ()
addToErrorView gui record@(Record _ (Errtoks tokens) _ _ _) = do
        (\model -> model `listStoreAppend` record) =<< (readIORef (errorModel gui))
        highlightTags gui (words tokens)

makeRecord :: Gui -> Error -> IO Record
makeRecord gui etype = do triggers <- readIORef (trigger gui)
                          tokens  <- readIORef (selectedTkn gui)
                          target <- entryGetText =<< (xmlGetWidget (xml gui) castToEntry "targetField")
                          comment <- entryGetText =<< (xmlGetWidget (xml gui) castToEntry "commentField")
                          return $ Record
                                    (Record_Attrs (Just $ token2string triggers) Nothing)
                                    (Errtoks $ token2string tokens)
                                    etype
                                    (target `truncateIfEqual` "Target Hypothesis" $ Target)
                                    (comment `truncateIfEqual` "Comment" $ Comment)

truncateIfEqual :: XmlContent a => String -> String -> (String -> a) -> Maybe a
truncateIfEqual s s' f | s == s'   = Nothing
                       | otherwise = Just $ f s

highlightTags :: Gui -> [String] -> IO ()
highlightTags gui tokens = do
        buffer <- textViewGetBuffer (corpusView gui)
        table <- textBufferGetTagTable buffer
        forM_ tokens (\idx -> do
                mtag <- table `textTagTableLookup` idx
                case mtag of
                        Nothing -> putStrLn "Warning: token tag not in table. Bad."
                        Just tag -> set tag [ textTagBackground := "#dd8888" ])

token2string :: [Token] -> String
token2string = unwords . (map tokenId)

clearBtnHandler :: Gui -> IO ()
clearBtnHandler gui =  do writeIORef (selectedTkn gui) []
                          writeIORef (trigger     gui) []
                          writeIORef (currentFocs gui) (selectedTkn gui)
                          (triggerBtn gui) `toggleButtonSetActive` False
                          (flip entrySetText $ "Target Hypothesis") =<< (xmlGetWidget (xml gui) castToEntry "targetField")
                          (flip entrySetText $ "Comment") =<< (xmlGetWidget (xml gui) castToEntry "commentField")
                          putTokensOnLabels gui

tagEventHandler :: Gui -> Token -> Old.Event -> TextIter -> IO ()
tagEventHandler gui (Token (Token_Attrs t) _ ) (Old.Motion _ _ _ _ _ _ _ _ ) _ = do
    label <- xmlGetWidget (xml gui) castToLabel "tindexLabel"
    label `labelSetText` (drop 1 t)
tagEventHandler gui t (Old.Button _ Old.SingleClick _ _ _ _ Old.LeftButton  _ _) _ = do
    tref <- readIORef (currentFocs gui)
    seltokens <- readIORef tref
    if t `elem` seltokens
       then writeIORef tref (t `delete` seltokens)
       else writeIORef tref (t:seltokens)
    putTokensOnLabels gui
tagEventHandler _ _ _ _ = return ()


triggerToggleHandler :: Gui -> IO ()
triggerToggleHandler gui = do active <- toggleButtonGetActive (triggerBtn gui)
                              if active
                                 then writeIORef (currentFocs gui) (trigger gui)
                                 else writeIORef (currentFocs gui) (selectedTkn gui)
