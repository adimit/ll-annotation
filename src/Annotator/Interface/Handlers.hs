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
         ts -> do paths <- treeSelectionGetSelectedRows =<< (treeViewGetSelection view)
                  case paths of
                    [path] -> do (EType _ etype) <- (\store ->treeStoreGetValue store path) =<< errorStore
                                 record <- makeRecord etype ts
                                 addToRecords gui record
                                 return ()
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

makeRecord :: Error -> [Token] -> IO Record
makeRecord etype ts = return $ Record (Record_Attrs Nothing Nothing) (Errtoks $ unwords . (map tokenId) $ ts) etype Nothing Nothing

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

