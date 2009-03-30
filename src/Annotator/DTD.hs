module Annotator.DTD where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.OneOfN


{-Type decls-}

data Corpus = Corpus Tokens Errors
            deriving (Eq,Show)
newtype Errors = Errors [Error] 		deriving (Eq,Show)
data Tokens = Tokens Tokens_Attrs [Token]
            deriving (Eq,Show)
data Tokens_Attrs = Tokens_Attrs
    { tokensCharlength :: String
    } deriving (Eq,Show)
data Errtoks = Errtoks
    { errtoksIdx :: String
    } deriving (Eq,Show)
data Error = Error Errtoks Type Context (Maybe Target)
                   (Maybe Comment)
           deriving (Eq,Show)
data Type = TypeForm Form
          | TypeGrammar Grammar
          | TypeSpelling Spelling
          | TypeGibberish Gibberish
          deriving (Eq,Show)
data Form = FormLexical Lexical
          | FormUnknown Unknown
          deriving (Eq,Show)
data Grammar = GrammarAgreement Agreement
             | GrammarOmission Omission
             | GrammarUnknown Unknown
             deriving (Eq,Show)
data Spelling = Spelling 		deriving (Eq,Show)
data Gibberish = Gibberish 		deriving (Eq,Show)
data Unknown = Unknown 		deriving (Eq,Show)
data Lexical = Lexical 		deriving (Eq,Show)
data Agreement = Agreement 		deriving (Eq,Show)
data Omission = Omission
    { omissionType :: Omission_type
    } deriving (Eq,Show)
data Omission_type = Omission_type_verb  |  Omission_type_subject
                      |  Omission_type_dirobject  |  Omission_type_indobject  | 
                     Omission_type_preposition  |  Omission_type_other
                   deriving (Eq,Show)
newtype Context = Context String 		deriving (Eq,Show)
newtype Comment = Comment String 		deriving (Eq,Show)
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
    toContents (Tokens as a) =
        [CElem (Elem "tokens" (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["tokens"]
        ; interior e $ return (Tokens (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <tokens>, "++)
instance XmlAttributes Tokens_Attrs where
    fromAttrs as =
        Tokens_Attrs
          { tokensCharlength = definiteA fromAttrToStr "tokens" "charlength" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "charlength" (tokensCharlength v)
        ]

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
    toContents (Error a b c d e) =
        [CElem (Elem "error" [] (toContents a ++ toContents b ++
                                 toContents c ++ maybe [] toContents d ++
                                 maybe [] toContents e)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["error"]
        ; interior e $ return (Error) `apply` parseContents
                       `apply` parseContents `apply` parseContents
                       `apply` optional parseContents `apply` optional parseContents
        } `adjustErr` ("in <error>, "++)

instance HTypeable Type where
    toHType x = Defined "type" [] []
instance XmlContent Type where
    toContents (TypeForm a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    toContents (TypeGrammar a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    toContents (TypeSpelling a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    toContents (TypeGibberish a) =
        [CElem (Elem "type" [] (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ [] _) <- element ["type"]
        ; interior e $ oneOf
            [ return (TypeForm) `apply` parseContents
            , return (TypeGrammar) `apply` parseContents
            , return (TypeSpelling) `apply` parseContents
            , return (TypeGibberish) `apply` parseContents
            ] `adjustErr` ("in <type>, "++)
        }

instance HTypeable Form where
    toHType x = Defined "form" [] []
instance XmlContent Form where
    toContents (FormLexical a) =
        [CElem (Elem "form" [] (toContents a) ) ()]
    toContents (FormUnknown a) =
        [CElem (Elem "form" [] (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ [] _) <- element ["form"]
        ; interior e $ oneOf
            [ return (FormLexical) `apply` parseContents
            , return (FormUnknown) `apply` parseContents
            ] `adjustErr` ("in <form>, "++)
        }

instance HTypeable Grammar where
    toHType x = Defined "grammar" [] []
instance XmlContent Grammar where
    toContents (GrammarAgreement a) =
        [CElem (Elem "grammar" [] (toContents a) ) ()]
    toContents (GrammarOmission a) =
        [CElem (Elem "grammar" [] (toContents a) ) ()]
    toContents (GrammarUnknown a) =
        [CElem (Elem "grammar" [] (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ [] _) <- element ["grammar"]
        ; interior e $ oneOf
            [ return (GrammarAgreement) `apply` parseContents
            , return (GrammarOmission) `apply` parseContents
            , return (GrammarUnknown) `apply` parseContents
            ] `adjustErr` ("in <grammar>, "++)
        }

instance HTypeable Spelling where
    toHType x = Defined "spelling" [] []
instance XmlContent Spelling where
    toContents Spelling =
        [CElem (Elem "spelling" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["spelling"]
        ; return Spelling
        } `adjustErr` ("in <spelling>, "++)

instance HTypeable Gibberish where
    toHType x = Defined "gibberish" [] []
instance XmlContent Gibberish where
    toContents Gibberish =
        [CElem (Elem "gibberish" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["gibberish"]
        ; return Gibberish
        } `adjustErr` ("in <gibberish>, "++)

instance HTypeable Unknown where
    toHType x = Defined "unknown" [] []
instance XmlContent Unknown where
    toContents Unknown =
        [CElem (Elem "unknown" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["unknown"]
        ; return Unknown
        } `adjustErr` ("in <unknown>, "++)

instance HTypeable Lexical where
    toHType x = Defined "lexical" [] []
instance XmlContent Lexical where
    toContents Lexical =
        [CElem (Elem "lexical" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["lexical"]
        ; return Lexical
        } `adjustErr` ("in <lexical>, "++)

instance HTypeable Agreement where
    toHType x = Defined "agreement" [] []
instance XmlContent Agreement where
    toContents Agreement =
        [CElem (Elem "agreement" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["agreement"]
        ; return Agreement
        } `adjustErr` ("in <agreement>, "++)

instance HTypeable Omission where
    toHType x = Defined "omission" [] []
instance XmlContent Omission where
    toContents as =
        [CElem (Elem "omission" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["omission"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <omission>, "++)
instance XmlAttributes Omission where
    fromAttrs as =
        Omission
          { omissionType = definiteA fromAttrToTyp "omission" "type" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "type" (omissionType v)
        ]

instance XmlAttrType Omission_type where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "verb" = Just Omission_type_verb
            translate "subject" = Just Omission_type_subject
            translate "dirobject" = Just Omission_type_dirobject
            translate "indobject" = Just Omission_type_indobject
            translate "preposition" = Just Omission_type_preposition
            translate "other" = Just Omission_type_other
            translate _ = Nothing
    toAttrFrTyp n Omission_type_verb = Just (n, str2attr "verb")
    toAttrFrTyp n Omission_type_subject = Just (n, str2attr "subject")
    toAttrFrTyp n Omission_type_dirobject = Just (n, str2attr "dirobject")
    toAttrFrTyp n Omission_type_indobject = Just (n, str2attr "indobject")
    toAttrFrTyp n Omission_type_preposition = Just (n, str2attr "preposition")
    toAttrFrTyp n Omission_type_other = Just (n, str2attr "other")

instance HTypeable Context where
    toHType x = Defined "context" [] []
instance XmlContent Context where
    toContents (Context a) =
        [CElem (Elem "context" [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["context"]
        ; interior e $ return (Context) `apply` (text `onFail` return "")
        } `adjustErr` ("in <context>, "++)

instance HTypeable Comment where
    toHType x = Defined "comment" [] []
instance XmlContent Comment where
    toContents (Comment a) =
        [CElem (Elem "comment" [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["comment"]
        ; interior e $ return (Comment) `apply` (text `onFail` return "")
        } `adjustErr` ("in <comment>, "++)

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
