{-# OPTIONS -fglasgow-exts #-}
module Annotator.Interface.Models where
    
import qualified Data.Tree as Tree
import qualified Data.List as List

import Annotator.DTD
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView
import Text.XML.HaXml.XmlContent

data EType =forall a b. XmlContent (a,b) =>
                  ENode { name :: String, payload :: (Maybe a -> b) }
           |forall a. XmlContent a =>
                  ELeaf { name :: String, leaf :: a }

                                                  
leafNode :: a -> Tree.Tree a
leafNode a = Tree.Node a []

errorStore :: IO (TreeStore EType)
errorStore = treeStoreNew 
    [ Tree.Node  (ENode "Error" (\t -> Error (Error_Attrs Nothing) t))
      [ Tree.Node (ENode "Spelling" (\t -> Error_Spelling (Spelling t)))
        [ leafNode $ ELeaf "Homonymy" (Spelling_Homonymy Homonymy)
        , leafNode $ ELeaf "Register" (Spelling_Register Register) 
        , leafNode $ ELeaf "Diacritics" (Spelling_Diacritics Diacritics) 
        , leafNode $ ELeaf "Local" (Spelling_Local Local) 
        ]
      , leafNode $ ENode "Grammar" (\t -> Error_Grammar (Grammar (Grammar_Attrs Nothing) t))
      , Tree.Node (ENode "Context" (\t -> Error_Context (Context t)))
        [ leafNode $ ELeaf "Lexical" (Context_Lexical Lexical) 
        , leafNode $ ELeaf "Idiom" (Context_Idiom Idiom)
        ]
      , leafNode $ ELeaf "Gibberish" (Error_Gibberish Gibberish)
      ]
   ]

leafType :: XmlContent a => a -> Maybe Error -> a
leafType c _ = c