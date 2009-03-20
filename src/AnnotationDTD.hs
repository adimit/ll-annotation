module Extsubset where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


{-Type decls-}

data Corpus = Corpus Tokens Errors
            deriving (Eq,Show)
newtype Errors = Errors [Error] 		deriving (Eq,Show)
newtype Tokens = Tokens [Token] 		deriving (Eq,Show)
data Errtoks = Errtoks
    { errtoksIdx :: String
    } deriving (Eq,Show)
data Error = Error Errtoks Type Target
           deriving (Eq,Show)
data Type = TypeForm Form
          | TypeMorph Morph
          | TypeSyn Syn
          | TypeLex Lex
          | TypeGrammar Grammar
          | TypeStyle Style
          | TypeTypo Typo
          | TypeUndef Undef
          deriving (Eq,Show)
data Form = Form Form_Attrs Comment
          deriving (Eq,Show)
data Form_Attrs = Form_Attrs
    { formCat :: Form_cat
    } deriving (Eq,Show)
data Form_cat = Form_cat_agglutination  |  Form_cat_register  | 
                Form_cat_diacritic  |  Form_cat_homonymy  |  Form_cat_speling
              deriving (Eq,Show)
data Morph = Morph Morph_Attrs Comment
           deriving (Eq,Show)
data Morph_Attrs = Morph_Attrs
    { morphCat :: Morph_cat
    } deriving (Eq,Show)
data Morph_cat = Morph_cat_derivation  |  Morph_cat_inflection  | 
                 Morph_cat_compounding
               deriving (Eq,Show)
data Syn = Syn Syn_Attrs Comment
         deriving (Eq,Show)
data Syn_Attrs = Syn_Attrs
    { synCat :: Syn_cat
    } deriving (Eq,Show)
data Syn_cat = Syn_cat_countability  |  Syn_cat_agreement  | 
               Syn_cat_worder  |  Syn_cat_wmissing  |  Syn_cat_wredundant  | 
               Syn_cat_cohesion
             deriving (Eq,Show)
data Lex = Lex Lex_Attrs Comment
         deriving (Eq,Show)
data Lex_Attrs = Lex_Attrs
    { lexCat :: Lex_cat
    } deriving (Eq,Show)
data Lex_cat = Lex_cat_meaning  |  Lex_cat_adjectivecompl  | 
               Lex_cat_adverbcompl  |  Lex_cat_verbcompl  |  Lex_cat_nouncompl  | 
               Lex_cat_prefab
             deriving (Eq,Show)
data Grammar = Grammar Grammar_Attrs Comment
             deriving (Eq,Show)
data Grammar_Attrs = Grammar_Attrs
    { grammarCat :: Grammar_cat
    } deriving (Eq,Show)
data Grammar_cat = Grammar_cat_class  |  Grammar_cat_aux  | 
                   Grammar_cat_gender  |  Grammar_cat_mode  |  Grammar_cat_number  | 
                   Grammar_cat_person  |  Grammar_cat_tense  |  Grammar_cat_voice  | 
                   Grammar_cat_euphony
                 deriving (Eq,Show)
data Style = Style Style_Attrs Comment
           deriving (Eq,Show)
data Style_Attrs = Style_Attrs
    { styleCat :: Style_cat
    } deriving (Eq,Show)
data Style_cat = Style_cat_unclear  |  Style_cat_heavy  | 
                 Style_cat_politeness  |  Style_cat_idiom
               deriving (Eq,Show)
newtype Typo = Typo Comment 		deriving (Eq,Show)
newtype Undef = Undef Comment 		deriving (Eq,Show)
data Comment = Comment Comment_Attrs String
             deriving (Eq,Show)
data Comment_Attrs = Comment_Attrs
    { commentAuthor :: String
    } deriving (Eq,Show)
newtype Target = Target String 		deriving (Eq,Show)
data Token = Token Token_Attrs String
           deriving (Eq,Show)
data Token_Attrs = Token_Attrs
    { tokenIdx :: String
    } deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Corpus where
    fromElem (CElem (Elem "corpus" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (Corpus a b), rest))
           (definite fromElem "<errors>" "corpus" ca))
        (definite fromElem "<tokens>" "corpus" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Corpus a b) =
        [CElem (Elem "corpus" [] (toElem a ++ toElem b))]
instance XmlContent Errors where
    fromElem (CElem (Elem "errors" [] c0):rest) =
        (\(a,ca)->
           (Just (Errors a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Errors a) =
        [CElem (Elem "errors" [] (concatMap toElem a))]
instance XmlContent Tokens where
    fromElem (CElem (Elem "tokens" [] c0):rest) =
        (\(a,ca)->
           (Just (Tokens a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Tokens a) =
        [CElem (Elem "tokens" [] (concatMap toElem a))]
instance XmlContent Errtoks where
    fromElem (CElem (Elem "errtoks" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "errtoks" (toAttrs as) [])]
instance XmlAttributes Errtoks where
    fromAttrs as =
        Errtoks
          { errtoksIdx = definiteA fromAttrToStr "errtoks" "idx" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "idx" (errtoksIdx v)
        ]
instance XmlContent Error where
    fromElem (CElem (Elem "error" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (Just (Error a b c), rest))
              (definite fromElem "<target>" "error" cb))
           (definite fromElem "<type>" "error" ca))
        (definite fromElem "<errtoks>" "error" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Error a b c) =
        [CElem (Elem "error" [] (toElem a ++ toElem b ++ toElem c))]
instance XmlContent Type where
    fromElem (CElem (Elem "type" [] c0):rest) =
        case (fromElem c0) of
        (Just a,_) -> (Just (TypeForm a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,_) -> (Just (TypeMorph a), rest)
                (_,_) ->
                        case (fromElem c0) of
                        (Just a,_) -> (Just (TypeSyn a), rest)
                        (_,_) ->
                                case (fromElem c0) of
                                (Just a,_) -> (Just (TypeLex a), rest)
                                (_,_) ->
                                        case (fromElem c0) of
                                        (Just a,_) -> (Just (TypeGrammar a), rest)
                                        (_,_) ->
                                                case (fromElem c0) of
                                                (Just a,_) -> (Just (TypeStyle a), rest)
                                                (_,_) ->
                                                        case (fromElem c0) of
                                                        (Just a,_) -> (Just (TypeTypo a), rest)
                                                        (_,_) ->
                                                                case (fromElem c0) of
                                                                (Just a,_) -> (Just (TypeUndef a), rest)
                                                                (_,_) ->
                                                                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (TypeForm a) = [CElem (Elem "type" [] (toElem a) )]
    toElem (TypeMorph a) = [CElem (Elem "type" [] (toElem a) )]
    toElem (TypeSyn a) = [CElem (Elem "type" [] (toElem a) )]
    toElem (TypeLex a) = [CElem (Elem "type" [] (toElem a) )]
    toElem (TypeGrammar a) = [CElem (Elem "type" [] (toElem a) )]
    toElem (TypeStyle a) = [CElem (Elem "type" [] (toElem a) )]
    toElem (TypeTypo a) = [CElem (Elem "type" [] (toElem a) )]
    toElem (TypeUndef a) = [CElem (Elem "type" [] (toElem a) )]
instance XmlContent Form where
    fromElem (CElem (Elem "Form" as c0):rest) =
        (\(a,ca)->
           (Just (Form (fromAttrs as) a), rest))
        (definite fromElem "<comment>" "Form" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Form as a) =
        [CElem (Elem "Form" (toAttrs as) (toElem a))]
instance XmlAttributes Form_Attrs where
    fromAttrs as =
        Form_Attrs
          { formCat = definiteA fromAttrToTyp "Form" "cat" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "cat" (formCat v)
        ]
instance XmlAttrType Form_cat where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "agglutination" = Just Form_cat_agglutination
            translate "register" = Just Form_cat_register
            translate "diacritic" = Just Form_cat_diacritic
            translate "homonymy" = Just Form_cat_homonymy
            translate "speling" = Just Form_cat_speling
            translate _ = Nothing
    toAttrFrTyp n Form_cat_agglutination = Just (n, str2attr "agglutination")
    toAttrFrTyp n Form_cat_register = Just (n, str2attr "register")
    toAttrFrTyp n Form_cat_diacritic = Just (n, str2attr "diacritic")
    toAttrFrTyp n Form_cat_homonymy = Just (n, str2attr "homonymy")
    toAttrFrTyp n Form_cat_speling = Just (n, str2attr "speling")
instance XmlContent Morph where
    fromElem (CElem (Elem "Morph" as c0):rest) =
        (\(a,ca)->
           (Just (Morph (fromAttrs as) a), rest))
        (definite fromElem "<comment>" "Morph" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Morph as a) =
        [CElem (Elem "Morph" (toAttrs as) (toElem a))]
instance XmlAttributes Morph_Attrs where
    fromAttrs as =
        Morph_Attrs
          { morphCat = definiteA fromAttrToTyp "Morph" "cat" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "cat" (morphCat v)
        ]
instance XmlAttrType Morph_cat where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "derivation" = Just Morph_cat_derivation
            translate "inflection" = Just Morph_cat_inflection
            translate "compounding" = Just Morph_cat_compounding
            translate _ = Nothing
    toAttrFrTyp n Morph_cat_derivation = Just (n, str2attr "derivation")
    toAttrFrTyp n Morph_cat_inflection = Just (n, str2attr "inflection")
    toAttrFrTyp n Morph_cat_compounding = Just (n, str2attr "compounding")
instance XmlContent Syn where
    fromElem (CElem (Elem "Syn" as c0):rest) =
        (\(a,ca)->
           (Just (Syn (fromAttrs as) a), rest))
        (definite fromElem "<comment>" "Syn" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Syn as a) =
        [CElem (Elem "Syn" (toAttrs as) (toElem a))]
instance XmlAttributes Syn_Attrs where
    fromAttrs as =
        Syn_Attrs
          { synCat = definiteA fromAttrToTyp "Syn" "cat" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "cat" (synCat v)
        ]
instance XmlAttrType Syn_cat where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "countability" = Just Syn_cat_countability
            translate "agreement" = Just Syn_cat_agreement
            translate "worder" = Just Syn_cat_worder
            translate "wmissing" = Just Syn_cat_wmissing
            translate "wredundant" = Just Syn_cat_wredundant
            translate "cohesion" = Just Syn_cat_cohesion
            translate _ = Nothing
    toAttrFrTyp n Syn_cat_countability = Just (n, str2attr "countability")
    toAttrFrTyp n Syn_cat_agreement = Just (n, str2attr "agreement")
    toAttrFrTyp n Syn_cat_worder = Just (n, str2attr "worder")
    toAttrFrTyp n Syn_cat_wmissing = Just (n, str2attr "wmissing")
    toAttrFrTyp n Syn_cat_wredundant = Just (n, str2attr "wredundant")
    toAttrFrTyp n Syn_cat_cohesion = Just (n, str2attr "cohesion")
instance XmlContent Lex where
    fromElem (CElem (Elem "Lex" as c0):rest) =
        (\(a,ca)->
           (Just (Lex (fromAttrs as) a), rest))
        (definite fromElem "<comment>" "Lex" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Lex as a) =
        [CElem (Elem "Lex" (toAttrs as) (toElem a))]
instance XmlAttributes Lex_Attrs where
    fromAttrs as =
        Lex_Attrs
          { lexCat = definiteA fromAttrToTyp "Lex" "cat" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "cat" (lexCat v)
        ]
instance XmlAttrType Lex_cat where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "meaning" = Just Lex_cat_meaning
            translate "adjectivecompl" = Just Lex_cat_adjectivecompl
            translate "adverbcompl" = Just Lex_cat_adverbcompl
            translate "verbcompl" = Just Lex_cat_verbcompl
            translate "nouncompl" = Just Lex_cat_nouncompl
            translate "prefab" = Just Lex_cat_prefab
            translate _ = Nothing
    toAttrFrTyp n Lex_cat_meaning = Just (n, str2attr "meaning")
    toAttrFrTyp n Lex_cat_adjectivecompl = Just (n, str2attr "adjectivecompl")
    toAttrFrTyp n Lex_cat_adverbcompl = Just (n, str2attr "adverbcompl")
    toAttrFrTyp n Lex_cat_verbcompl = Just (n, str2attr "verbcompl")
    toAttrFrTyp n Lex_cat_nouncompl = Just (n, str2attr "nouncompl")
    toAttrFrTyp n Lex_cat_prefab = Just (n, str2attr "prefab")
instance XmlContent Grammar where
    fromElem (CElem (Elem "Grammar" as c0):rest) =
        (\(a,ca)->
           (Just (Grammar (fromAttrs as) a), rest))
        (definite fromElem "<comment>" "Grammar" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Grammar as a) =
        [CElem (Elem "Grammar" (toAttrs as) (toElem a))]
instance XmlAttributes Grammar_Attrs where
    fromAttrs as =
        Grammar_Attrs
          { grammarCat = definiteA fromAttrToTyp "Grammar" "cat" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "cat" (grammarCat v)
        ]
instance XmlAttrType Grammar_cat where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "class" = Just Grammar_cat_class
            translate "aux" = Just Grammar_cat_aux
            translate "gender" = Just Grammar_cat_gender
            translate "mode" = Just Grammar_cat_mode
            translate "number" = Just Grammar_cat_number
            translate "person" = Just Grammar_cat_person
            translate "tense" = Just Grammar_cat_tense
            translate "voice" = Just Grammar_cat_voice
            translate "euphony" = Just Grammar_cat_euphony
            translate _ = Nothing
    toAttrFrTyp n Grammar_cat_class = Just (n, str2attr "class")
    toAttrFrTyp n Grammar_cat_aux = Just (n, str2attr "aux")
    toAttrFrTyp n Grammar_cat_gender = Just (n, str2attr "gender")
    toAttrFrTyp n Grammar_cat_mode = Just (n, str2attr "mode")
    toAttrFrTyp n Grammar_cat_number = Just (n, str2attr "number")
    toAttrFrTyp n Grammar_cat_person = Just (n, str2attr "person")
    toAttrFrTyp n Grammar_cat_tense = Just (n, str2attr "tense")
    toAttrFrTyp n Grammar_cat_voice = Just (n, str2attr "voice")
    toAttrFrTyp n Grammar_cat_euphony = Just (n, str2attr "euphony")
instance XmlContent Style where
    fromElem (CElem (Elem "Style" as c0):rest) =
        (\(a,ca)->
           (Just (Style (fromAttrs as) a), rest))
        (definite fromElem "<comment>" "Style" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Style as a) =
        [CElem (Elem "Style" (toAttrs as) (toElem a))]
instance XmlAttributes Style_Attrs where
    fromAttrs as =
        Style_Attrs
          { styleCat = definiteA fromAttrToTyp "Style" "cat" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "cat" (styleCat v)
        ]
instance XmlAttrType Style_cat where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "unclear" = Just Style_cat_unclear
            translate "heavy" = Just Style_cat_heavy
            translate "politeness" = Just Style_cat_politeness
            translate "idiom" = Just Style_cat_idiom
            translate _ = Nothing
    toAttrFrTyp n Style_cat_unclear = Just (n, str2attr "unclear")
    toAttrFrTyp n Style_cat_heavy = Just (n, str2attr "heavy")
    toAttrFrTyp n Style_cat_politeness = Just (n, str2attr "politeness")
    toAttrFrTyp n Style_cat_idiom = Just (n, str2attr "idiom")
instance XmlContent Typo where
    fromElem (CElem (Elem "Typo" [] c0):rest) =
        (\(a,ca)->
           (Just (Typo a), rest))
        (definite fromElem "<comment>" "Typo" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Typo a) =
        [CElem (Elem "Typo" [] (toElem a))]
instance XmlContent Undef where
    fromElem (CElem (Elem "Undef" [] c0):rest) =
        (\(a,ca)->
           (Just (Undef a), rest))
        (definite fromElem "<comment>" "Undef" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Undef a) =
        [CElem (Elem "Undef" [] (toElem a))]
instance XmlContent Comment where
    fromElem (CElem (Elem "comment" as c0):rest) =
        (\(a,ca)->
           (Just (Comment (fromAttrs as) a), rest))
        (definite fromText "text" "comment" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Comment as a) =
        [CElem (Elem "comment" (toAttrs as) (toText a))]
instance XmlAttributes Comment_Attrs where
    fromAttrs as =
        Comment_Attrs
          { commentAuthor = definiteA fromAttrToStr "comment" "author" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "author" (commentAuthor v)
        ]
instance XmlContent Target where
    fromElem (CElem (Elem "target" [] c0):rest) =
        (\(a,ca)->
           (Just (Target a), rest))
        (definite fromText "text" "target" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Target a) =
        [CElem (Elem "target" [] (toText a))]
instance XmlContent Token where
    fromElem (CElem (Elem "token" as c0):rest) =
        (\(a,ca)->
           (Just (Token (fromAttrs as) a), rest))
        (definite fromText "text" "token" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Token as a) =
        [CElem (Elem "token" (toAttrs as) (toText a))]
instance XmlAttributes Token_Attrs where
    fromAttrs as =
        Token_Attrs
          { tokenIdx = definiteA fromAttrToStr "token" "idx" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "idx" (tokenIdx v)
        ]


{-Done-}
