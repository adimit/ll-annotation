{-# OPTIONS -fglasgow-exts #-}
module Annotator.Interface.Models where
    
import qualified Data.Tree as Tree
import qualified Data.List as List

import Annotator.DTD
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView
import Text.XML.HaXml.XmlContent

data EType = forall a b. XmlContent (a,b) => EType { name :: String , payload :: (Maybe a -> b) }
                                                  
leafNode :: a -> Tree.Tree a
leafNode a = Tree.Node a []

errorStore :: IO (TreeStore EType)
errorStore = treeStoreNew 
    [ Tree.Node  (EType "Error" (Error (Error_Attrs Nothing)))
      [ Tree.Node (EType "Spelling" (\t -> Error_Spelling (Spelling t)))
        [ leafNode $ EType "Homonymy" (leafType $ (Spelling_Homonymy Homonymy)) 
        , leafNode $ EType "Register" (leafType $ (Spelling_Register Register)) 
        , leafNode $ EType "Diacritics" (leafType $ (Spelling_Diacritics Diacritics)) 
        , leafNode $ EType "Local" (leafType $ (Spelling_Local Local)) 
        ]
      , leafNode $ EType "Grammar" (\t -> Error_Grammar (Grammar (Grammar_Attrs Nothing) t))
      , Tree.Node (EType "Context" (\t -> Error_Context (Context t)))
        [ leafNode $ EType "Lexical" (leafType $ (Context_Lexical Lexical)) 
        , leafNode $ EType "Idiom" (leafType $ (Context_Idiom Idiom)) 
        ]
      , leafNode $ EType "Gibberish" (leafType $ (Error_Gibberish Gibberish))
      ]
   ]

leafType :: XmlContent a => a -> Maybe Error -> a
leafType c _ = c