module Annotator.Interface.Models where
    
import qualified Data.Tree as Tree
import qualified Data.List as List

import Annotator.DTD
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView
import Text.XML.HaXml.XmlContent

data (XmlContent a) => EType a =  EType { name :: String , xmlContent :: Maybe a }

grammarStore :: IO (TreeStore (EType Grammar))
grammarStore = treeStoreNew
        [ Tree.Node
           { Tree.rootLabel = EType { name = "Omission", xmlContent = Nothing }
           , Tree.subForest = [ leafNode EType { name = "subject" 
                                               , xmlContent = Just $ GrammarOmission (Omission Omission_type_subject)}
                              , leafNode EType { name = "verb" 
                                               , xmlContent = Just $ GrammarOmission (Omission Omission_type_verb)}
                              , leafNode EType { name = "direct object" 
                                               , xmlContent = Just $ GrammarOmission (Omission Omission_type_dirobject)}
                              , leafNode EType { name = "indirect object" 
                                               , xmlContent = Just $ GrammarOmission (Omission Omission_type_indobject)}
                              , leafNode EType { name = "preposition" 
                                               , xmlContent = Just $ GrammarOmission (Omission Omission_type_preposition)}
                              , leafNode EType { name = "other" 
                                               , xmlContent = Just $ GrammarOmission (Omission Omission_type_other)}
                              ]
           }
        , leafNode EType { name = "Agreement", xmlContent = Just $ GrammarAgreement Agreement }
        , leafNode EType { name = "Unknown", xmlContent = Just $ GrammarUnknown Unknown }
        ]

formStore = treeStoreNew
        [ leafNode EType { name = "Lexical", xmlContent = Just $ FormLexical Lexical }
        , leafNode EType { name = "Unknown", xmlContent = Just $ FormUnknown Unknown }
        ]

leafNode :: a -> Tree.Tree a
leafNode a = Tree.Node { Tree.rootLabel = a, Tree.subForest = [] }

