module Annotator.Interface.Models where
    
import qualified Data.Tree as Tree
import qualified Data.List as List

import Annotator.DTD
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView
import Text.XML.HaXml.XmlContent

data (XmlContent a) => EType a =  EType { name :: String , xmlContent :: a }


errorStore :: IO (TreeStore (EType Error))
errorStore = treeStoreNew
    [ leafNode EType { name = "Error"
                     , xmlContent = Error (Error_Attrs Nothing) Nothing
                     }
    ]

leafNode :: a -> Tree.Tree a
leafNode a = Tree.Node { Tree.rootLabel = a, Tree.subForest = [] }
