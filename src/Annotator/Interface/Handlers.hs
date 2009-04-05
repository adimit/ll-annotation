module Annotator.Interface.Handlers where

import Annotator.Interface.Types
import Annotator.Interface.Util
import Annotator.Interface.Models
import Annotator.DTD
import Data.IORef
import Data.List
import GHC.List hiding (span)
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Old

recordHandler :: Gui -> TreeView -> IO ()
recordHandler gui view = 
    do tokens <- readIORef (selectedTkn gui)
       case tokens of
         [] -> putStrLn "Please select some tokens."
         _ -> do paths <- treeSelectionGetSelectedRows =<< (treeViewGetSelection view)
                 case paths of
                   [path] -> do (EType _ etype) <- (\store ->treeStoreGetValue store path) =<< errorStore
                                record <- makeRecord gui etype
                                addToRecords gui record
                                clearBtnHandler gui
                   _ -> putStrLn "Warning: Select one error type first."
       
addToRecords :: Gui -> Record -> IO ()
addToRecords gui err = do ref <- readIORef (xmlDocument gui)
                          case ref of
                              Nothing  -> putStrLn "Warning, empty document!"
                              Just doc -> writeIORef (xmlDocument gui) (Just $ ate err doc) 
                              where ate e c@(Corpus ts (Errors es)) = 
                                                             if e `elem` es
                                                                then c
                                                                else Corpus ts (Errors (e:es))

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