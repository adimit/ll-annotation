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
      , Tree.Node (EType "Grammar" (Error  (Just $ Error_Grammar (Grammar  Nothing))))
        [ Tree.Node (EType "Agreement" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement Nothing))))))
          [ leafNode $ EType "Number" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement (Just $ Agreement_Number Number))))))
          , leafNode $ EType "Gender" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement (Just $ Agreement_Gender Gender))))))
          , leafNode $ EType "Case" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement (Just $ Agreement_Case Case))))))
          , leafNode $ EType "Person" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement (Just $ Agreement_Person Person))))))
          , leafNode $ EType "Tense" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement (Just $ Agreement_Tense Tense))))))
          ]
        , leafNode $ EType "Redundancy" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Redundancy Redundancy))))
        , leafNode $ EType "Valency" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Valency Valency))))
        , leafNode $ EType "Complementation" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Complementation Complementation))))
        , leafNode $ EType "Voice" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Voice Voice))))
        , Tree.Node (EType "Word Order" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Wo (Wo Nothing))))))
          [ leafNode $ EType "Internal" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Wo (Wo (Just $ Wo_Internal Internal))))))
          , leafNode $ EType "External" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Wo (Wo (Just $ Wo_External External))))))
          ]
        , Tree.Node (EType "Morphology" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Morphology (Morphology Nothing))))))
          [ leafNode $ EType "Derivation" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Morphology (Morphology (Just $ Morphology_Derivation Derivation))))))
          , leafNode $ EType "Compounding" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Morphology (Morphology (Just $ Morphology_Compounding Compounding))))))
          , leafNode $ EType "Inflection" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Morphology (Morphology (Just $ Morphology_Inflection Inflection))))))
          ]
        , Tree.Node (EType "Omission" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omission (Omission Nothing))))))
          [ leafNode $ EType "Subject" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omission (Omission (Just $ Omission_Subject Subject))))))
          , leafNode $ EType "Predicate" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omission (Omission (Just $ Omission_Predicate Predicate))))))
          , leafNode $ EType "Article" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omission (Omission (Just $ Omission_Article Article))))))
          , leafNode $ EType "Preposition" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omission (Omission (Just $ Omission_Preposition Preposition))))))
          , Tree.Node (EType "Object" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omission (Omission (Just $ Omission_Object (Object Nothing))))))))
            [ leafNode $ EType "direct" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omission (Omission (Just $ Omission_Object (Object (Just $ Object_Direct Direct))))))))
            , leafNode $ EType "indirect" (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omission (Omission (Just $ Omission_Object (Object (Just $ Object_Indirect Indirect))))))))
            ]
          ]
        ]
      , Tree.Node (EType "Context" (Error  (Just $ Error_Context (Context  Nothing))))
        [ leafNode $ EType "Lexical" (Error  (Just $ Error_Context (Context  (Just $ Context_Lexical Lexical))))
        , leafNode $ EType "Idiom" (Error  (Just $ Error_Context (Context  (Just $ Context_Idiom Idiom))))
        ]
      , leafNode $ EType "Gibberish" (Error  (Just $ Error_Gibberish Gibberish))
      ]
   ]
