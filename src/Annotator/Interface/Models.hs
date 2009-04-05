{-# OPTIONS -fglasgow-exts #-}
module Annotator.Interface.Models where

import qualified Data.Tree as Tree
import qualified Data.List as List

import Annotator.DTD
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView
import Text.XML.HaXml.XmlContent

data (XmlContent a) => EType a = EType { name :: String, payload :: a }

leafNode :: a -> Tree.Tree a
leafNode a = Tree.Node a []

errorStore :: IO (TreeStore (EType Error))
errorStore = treeStoreNew
    [ Tree.Node (EType "Error" (Error  Nothing))
      [ Tree.Node (EType "Spelling" (Error  (Just $ Error_Spelling (Spelling Nothing))))
        [ leafNode $ EType "Homonymy" (Error  (Just $ Error_Spelling (Spelling (Just $ Spelling_Homonymy Homonymy))))
        , leafNode $ EType "Register" (Error  (Just $ Error_Spelling (Spelling (Just $ Spelling_Register Register))))
        , leafNode $ EType "Diacritics" (Error  (Just $ Error_Spelling (Spelling (Just $ Spelling_Diacritics Diacritics))))
        , leafNode $ EType "Local" (Error  (Just $ Error_Spelling (Spelling (Just $ Spelling_Local Local))))
        ]
      , leafNode $ EType "Grammar" (Error  (Just $ Error_Grammar (Grammar  Nothing)))
      , Tree.Node (EType "Context" (Error  (Just $ Error_Context (Context  Nothing))))
        [ leafNode $ EType "Lexical" (Error  (Just $ Error_Context (Context  (Just $ Context_Lexical Lexical))))
        , leafNode $ EType "Idiom" (Error  (Just $ Error_Context (Context  (Just $ Context_Idiom Idiom))))
        ]
      , leafNode $ EType "Gibberish" (Error  (Just $ Error_Gibberish Gibberish))
      ]
   ]
