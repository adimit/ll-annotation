module Annotator.Interface.Handlers where

import Annotator.Interface.Types
import Annotator.Interface.Util
import Annotator.DTD
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

recordHandler gui view = do row <- (treeViewGetSelection view >>= treeSelectionGetSelectedRows)
                            tokens <- readIORef (selectedTkn gui)
                            case tokens of
                                [] -> putStrLn "Please select some tokens."
                                ts -> case row of
                                          [path] -> undefined

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

