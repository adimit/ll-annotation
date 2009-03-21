module Annotator.DTD where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.OneOfN


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

instance HTypeable Corpus where
    toHType x = Defined "corpus" [] []
instance XmlContent Corpus where
    toContents (Corpus a b) =
        [CElem (Elem "corpus" [] (toContents a ++ toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["corpus"]
        ; interior e $ return (Corpus) `apply` parseContents
                       `apply` parseContents
        } `adjustErr` ("in <corpus>, "++)

instance HTypeable Errors where
    toHType x = Defined "errors" [] []
instance XmlContent Errors where
    toContents (Errors a) =
        [CElem (Elem "errors" [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["errors"]
        ; interior e $ return (Errors) `apply` many parseContents
        } `adjustErr` ("in <errors>, "++)

instance HTypeable Tokens where
    toHType x = Defined "tokens" [] []
instance XmlContent Tokens where
    toContents (Tokens a) =
        [CElem (Elem "tokens" [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["tokens"]
        ; interior e $ return (Tokens) `apply` many parseContents
        } `adjustErr` ("in <tokens>, "++)

instance HTypeable Errtoks where
    toHType x = Defined "errtoks" [] []
instance XmlContent Errtoks where
    toContents as =
        [CElem (Elem "errtoks" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["errtoks"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <errtoks>, "++)
instance XmlAttributes Errtoks where
    fromAttrs as =
        Errtoks
          { errtoksIdx = definiteA fromAttrToStr "errtoks" "idx" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "idx" (errtoksIdx v)
        ]

instance HTypeable Error where
    toHType x = Defined "error" [] []
instance XmlContent Error where
    toContents (Error a b c) =
        [CElem (Elem "error" [] (toContents a ++ toContents b ++
                                 toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["error"]
        ; interior e $ return (Error) `apply` parseContents
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <error>, "++)

instance HTypeable Type where
    toHType x = Defined "type" [] []
instance XmlContent Type where
    toContents (TypeForm a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    toContents (TypeMorph a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    toContents (TypeSyn a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    toContents (TypeLex a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    toContents (TypeGrammar a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    toContents (TypeStyle a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    toContents (TypeTypo a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    toContents (TypeUndef a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ [] _) <- element ["type"]
        ; interior e $ oneOf
            [ return (TypeForm) `apply` parseContents
            , return (TypeMorph) `apply` parseContents
            , return (TypeSyn) `apply` parseContents
            , return (TypeLex) `apply` parseContents
            , return (TypeGrammar) `apply` parseContents
            , return (TypeStyle) `apply` parseContents
            , return (TypeTypo) `apply` parseContents
            , return (TypeUndef) `apply` parseContents
            ] `adjustErr` ("in <type>, "++)
        }

instance HTypeable Form where
    toHType x = Defined "Form" [] []
instance XmlContent Form where
    toContents (Form as a) =
        [CElem (Elem "Form" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Form"]
        ; interior e $ return (Form (fromAttrs as)) `apply` parseContents
        } `adjustErr` ("in <Form>, "++)
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

instance HTypeable Morph where
    toHType x = Defined "Morph" [] []
instance XmlContent Morph where
    toContents (Morph as a) =
        [CElem (Elem "Morph" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Morph"]
        ; interior e $ return (Morph (fromAttrs as)) `apply` parseContents
        } `adjustErr` ("in <Morph>, "++)
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

instance HTypeable Syn where
    toHType x = Defined "Syn" [] []
instance XmlContent Syn where
    toContents (Syn as a) =
        [CElem (Elem "Syn" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Syn"]
        ; interior e $ return (Syn (fromAttrs as)) `apply` parseContents
        } `adjustErr` ("in <Syn>, "++)
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

instance HTypeable Lex where
    toHType x = Defined "Lex" [] []
instance XmlContent Lex where
    toContents (Lex as a) =
        [CElem (Elem "Lex" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Lex"]
        ; interior e $ return (Lex (fromAttrs as)) `apply` parseContents
        } `adjustErr` ("in <Lex>, "++)
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

instance HTypeable Grammar where
    toHType x = Defined "Grammar" [] []
instance XmlContent Grammar where
    toContents (Grammar as a) =
        [CElem (Elem "Grammar" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Grammar"]
        ; interior e $ return (Grammar (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <Grammar>, "++)
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

instance HTypeable Style where
    toHType x = Defined "Style" [] []
instance XmlContent Style where
    toContents (Style as a) =
        [CElem (Elem "Style" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Style"]
        ; interior e $ return (Style (fromAttrs as)) `apply` parseContents
        } `adjustErr` ("in <Style>, "++)
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

instance HTypeable Typo where
    toHType x = Defined "Typo" [] []
instance XmlContent Typo where
    toContents (Typo a) =
        [CElem (Elem "Typo" [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Typo"]
        ; interior e $ return (Typo) `apply` parseContents
        } `adjustErr` ("in <Typo>, "++)

instance HTypeable Undef where
    toHType x = Defined "Undef" [] []
instance XmlContent Undef where
    toContents (Undef a) =
        [CElem (Elem "Undef" [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Undef"]
        ; interior e $ return (Undef) `apply` parseContents
        } `adjustErr` ("in <Undef>, "++)

instance HTypeable Comment where
    toHType x = Defined "comment" [] []
instance XmlContent Comment where
    toContents (Comment as a) =
        [CElem (Elem "comment" (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["comment"]
        ; interior e $ return (Comment (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <comment>, "++)
instance XmlAttributes Comment_Attrs where
    fromAttrs as =
        Comment_Attrs
          { commentAuthor = definiteA fromAttrToStr "comment" "author" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "author" (commentAuthor v)
        ]

instance HTypeable Target where
    toHType x = Defined "target" [] []
instance XmlContent Target where
    toContents (Target a) =
        [CElem (Elem "target" [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["target"]
        ; interior e $ return (Target) `apply` (text `onFail` return "")
        } `adjustErr` ("in <target>, "++)

instance HTypeable Token where
    toHType x = Defined "token" [] []
instance XmlContent Token where
    toContents (Token as a) =
        [CElem (Elem "token" (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["token"]
        ; interior e $ return (Token (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <token>, "++)
instance XmlAttributes Token_Attrs where
    fromAttrs as =
        Token_Attrs
          { tokenIdx = definiteA fromAttrToStr "token" "idx" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "idx" (tokenIdx v)
        ]



{-Done-}
