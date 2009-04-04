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
import Text.XML.HaXml.XmlContent

recordHandler :: Gui -> TreeView -> IO ()
recordHandler gui view = 
    do tokens <- readIORef (selectedTkn gui)
       case tokens of
         [] -> putStrLn "Please select some tokens."
         ts -> do Just iter <- (treeViewGetSelection view >>= treeSelectionGetSelected) 
                  etypelist <- (\store -> recurseToParent (Just iter) store) =<< errorStore
                  return ()
       
       
recurseToParent :: (Maybe TreeIter) -> TreeStore EType -> IO [EType]
recurseToParent Nothing _ = return []
recurseToParent (Just iter) store = 
    do parent <- treeModelIterParent store iter 
       etype <- (treeStoreGetValue store) =<< (store `treeModelGetPath` iter) 
       recurseToParent parent store >>= \t -> return (etype:t)
       
foldElist :: [EType] -> Error
foldElist ((ENode _ t nothing):es) = foldElist' (t nothing) es
foldElist ((ELeaf _ t):es) = foldElist' t es
foldElist _ = error "WTF?"

foldElist' :: XmlContent a => a -> [EType] -> Error
foldElist' = undefined

addToRecords :: Gui -> Record -> IO ()
addToRecords = undefined

makeRecord :: Error -> IO Record
makeRecord = undefined

clearBtnHandler :: Gui -> IO ()
clearBtnHandler gui =  do writeIORef (selectedTkn gui) []
                          putTokensOnLabel gui (tokenLabel gui)

tagEventHandler :: Gui -> Token -> Old.Event -> TextIter -> IO ()
tagEventHandler gui t (Old.Button _ Old.SingleClick _ _ _ _ Old.LeftButton  _ _) _ = do
    seltokens <- readIORef (selectedTkn gui)
    if t `elem` seltokens
       then writeIORef (selectedTkn gui) (t `delete` seltokens)
       else writeIORef (selectedTkn gui) (t:seltokens)
    putTokensOnLabel gui (tokenLabel gui)
tagEventHandler _ _ _ _ = return ()

