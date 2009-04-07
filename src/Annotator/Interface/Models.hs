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
        , leafNode $ EType "Local Variation" (Error  (Just $ Error_Spelling (Spelling (Just $ Spelling_Local Local))))
        ]
      , Tree.Node (EType "Grammar"           (Error  (Just $ Error_Grammar (Grammar  Nothing))))
        [ Tree.Node (EType "Agreement"       (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement Nothing))))))
          [ leafNode $ EType "Number"        (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement (Just $ Agreement_Number Number))))))
          , leafNode $ EType "Gender"        (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement (Just $ Agreement_Gender Gender))))))
          , leafNode $ EType "Case"          (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement (Just $ Agreement_Case Case))))))
          , leafNode $ EType "Person"        (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement (Just $ Agreement_Person Person))))))
          , leafNode $ EType "Tense"         (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Agreement (Agreement (Just $ Agreement_Tense Tense))))))
          ]
        , leafNode $ EType "Redundancy"      (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Redundancy Redundancy))))
        , leafNode $ EType "Countability"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Countability Countability))))
        , leafNode $ EType "Collocation"     (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Collocation Collocation))))
        , leafNode $ EType "Voice"           (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Voice Voice))))
        , leafNode $ EType "Verb Tense"      (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Verbtense Verbtense))))
        , Tree.Node (EType "Word Order"      (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Wo (Wo Nothing))))))
          [ leafNode $ EType "Internal"      (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Wo (Wo (Just $ Wo_Internal Internal))))))
          , leafNode $ EType "External"      (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Wo (Wo (Just $ Wo_External External))))))
          ]
        , Tree.Node (EType "Morphology"      (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Morphology (Morphology Nothing))))))
          [ leafNode $ EType "Derivation"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Morphology (Morphology (Just $ Morphology_Derivation Derivation))))))
          , leafNode $ EType "Compounding"   (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Morphology (Morphology (Just $ Morphology_Compounding Compounding))))))
          , leafNode $ EType "Inflection"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Morphology (Morphology (Just $ Morphology_Inflection Inflection))))))
          ]
        , Tree.Node (EType "Omitted"         (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted Nothing))))))
          [ leafNode $ EType "Subject"       (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Subject Subject))))))
          , leafNode $ EType "Predicate"     (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Predicate Predicate))))))
          , leafNode $ EType "Determiner"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Determiner Determiner))))))
          , leafNode $ EType "Pronoun"       (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Pronoun Pronoun))))))
          , leafNode $ EType "Adverb"        (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Adverb Adverb))))))
          , leafNode $ EType "Preposition"   (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Preposition Preposition))))))
          , leafNode $ EType "Noun"          (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Noun Noun))))))
          , leafNode $ EType "Conjunction"   (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Conjunction Conjunction))))))
          , leafNode $ EType "Adjective"     (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Adjective Adjective))))))
          , leafNode $ EType "Verb"          (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Verb Verb))))))
          , Tree.Node (EType "Object"        (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Object (Object Nothing))))))))
            [ leafNode $ EType "direct"      (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Object (Object (Just $ Object_Direct Direct))))))))
            , leafNode $ EType "indirect"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Object (Object (Just $ Object_Indirect Indirect))))))))
            ]
          ]
        , Tree.Node (EType "Replace"         (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace Nothing))))))
          [ leafNode $ EType " Determiner"     (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Determiner Determiner))))))
          , leafNode $ EType " Pronoun"        (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Pronoun Pronoun))))))
          , leafNode $ EType " Adverb"         (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Adverb Adverb))))))
          , leafNode $ EType " Preposition"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Preposition Preposition))))))
          , leafNode $ EType " Noun"           (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Noun Noun))))))
          , leafNode $ EType " Conjunction"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Conjunction Conjunction))))))
          , leafNode $ EType " Adjective"      (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Adjective Adjective))))))
          , leafNode $ EType " Verb"           (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Verb Verb))))))
          ]
        ]
      , Tree.Node (EType "Context" (Error  (Just $ Error_Context (Context  Nothing))))
        [ leafNode $ EType "Semantics" (Error  (Just $ Error_Context (Context  (Just $ Context_Semantics Semantics))))
        , leafNode $ EType "Idiom" (Error  (Just $ Error_Context (Context  (Just $ Context_Idiom Idiom))))
        , leafNode $ EType "Verb tense" (Error  (Just $ Error_Context (Context  (Just $ Context_Verbtense Verbtense))))
        , leafNode $ EType "Redundancy" (Error  (Just $ Error_Context (Context  (Just $ Context_Redundancy Redundancy))))
        , Tree.Node (EType "Omitted"         (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted Nothing))))))
          [ leafNode $ EType "Subject"       (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Subject Subject))))))
          , leafNode $ EType "Predicate"     (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Predicate Predicate))))))
          , leafNode $ EType "Determiner"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Determiner Determiner))))))
          , leafNode $ EType "Pronoun"       (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Pronoun Pronoun))))))
          , leafNode $ EType "Adverb"        (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Adverb Adverb))))))
          , leafNode $ EType "Preposition"   (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Preposition Preposition))))))
          , leafNode $ EType "Noun"          (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Noun Noun))))))
          , leafNode $ EType "Conjunction"   (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Conjunction Conjunction))))))
          , leafNode $ EType "Adjective"     (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Adjective Adjective))))))
          , leafNode $ EType "Verb"          (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Verb Verb))))))
          , Tree.Node (EType "Object"        (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Object (Object Nothing))))))))
            [ leafNode $ EType "direct"      (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Object (Object (Just $ Object_Direct Direct))))))))
            , leafNode $ EType "indirect"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Omitted (Omitted (Just $ Omitted_Object (Object (Just $ Object_Indirect Indirect))))))))
            ]
          ]
        , Tree.Node (EType "Replace"         (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace Nothing))))))
          [ leafNode $ EType " Determiner"     (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Determiner Determiner))))))
          , leafNode $ EType " Pronoun"        (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Pronoun Pronoun))))))
          , leafNode $ EType " Adverb"         (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Adverb Adverb))))))
          , leafNode $ EType " Preposition"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Preposition Preposition))))))
          , leafNode $ EType " Noun"           (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Noun Noun))))))
          , leafNode $ EType " Conjunction"    (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Conjunction Conjunction))))))
          , leafNode $ EType " Adjective"      (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Adjective Adjective))))))
          , leafNode $ EType " Verb"           (Error  (Just $ Error_Grammar (Grammar  (Just $ Grammar_Replace (Replace (Just $ Replace_Verb Verb))))))
          ]
        ]
      ]
   ]
