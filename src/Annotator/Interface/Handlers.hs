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
import qualified Graphics.UI.Gtk.Gdk.Events as Old

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
                   _ -> putStrLn "Warning: Select one error type first."
       
addToErrorView :: Gui -> Record -> IO ()
addToErrorView gui record =  
        (\model -> model `listStorePrepend` record) =<< (readIORef (errorModel gui))
                                             
makeRecord :: Gui -> Error -> IO Record
makeRecord gui etype = do triggers <- readIORef (trigger gui)
                          tokens  <- readIORef (selectedTkn gui)
                          return $ Record (Record_Attrs (Just $ token2string triggers) Nothing) (Errtoks $ token2string tokens) etype Nothing Nothing

token2string :: [Token] -> String
token2string = unwords . (map tokenId)

clearBtnHandler :: Gui -> IO ()
clearBtnHandler gui =  do writeIORef (selectedTkn gui) []
                          writeIORef (trigger     gui) []
                          writeIORef (currentFocs gui) (selectedTkn gui)
                          (triggerBtn gui) `toggleButtonSetActive` False
                          putTokensOnLabels gui

tagEventHandler :: Gui -> Token -> Old.Event -> TextIter -> IO ()
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