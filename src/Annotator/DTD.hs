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
data Error = Error Errtoks Type Context (Maybe Target)
                   (Maybe Comment)
           deriving (Eq,Show)
data Type = TypeForm Form
          | TypeGrammar Grammar
          | TypeSpelling Spelling
          deriving (Eq,Show)
data Form = Form
    { formCat :: Form_cat
    } deriving (Eq,Show)
data Form_cat = Form_cat_agglutination  |  Form_cat_register  | 
                Form_cat_diacritic  |  Form_cat_homonymy  |  Form_cat_speling
              deriving (Eq,Show)
data Grammar = Grammar
    { grammarCat :: Grammar_cat
    } deriving (Eq,Show)
data Grammar_cat = Grammar_cat_class  |  Grammar_cat_aux  | 
                   Grammar_cat_gender  |  Grammar_cat_mode  |  Grammar_cat_number  | 
                   Grammar_cat_person  |  Grammar_cat_tense  |  Grammar_cat_voice  | 
                   Grammar_cat_euphony
                 deriving (Eq,Show)
data Spelling = Spelling 		deriving (Eq,Show)
newtype Context = Context String 		deriving (Eq,Show)
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
    parseContents = do 
        { e@(Elem _ [] _) <- element ["type"]
        ; interior e $ oneOf
            [ return (TypeForm) `apply` parseContents
            , return (TypeGrammar) `apply` parseContents
            , return (TypeSpelling) `apply` parseContents
            ] `adjustErr` ("in <type>, "++)
        }

instance HTypeable Form where
    toHType x = Defined "Form" [] []
instance XmlContent Form where
    toContents as =
        [CElem (Elem "Form" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["Form"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <Form>, "++)
instance XmlAttributes Form where
    fromAttrs as =
        Form
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

instance HTypeable Grammar where
    toHType x = Defined "Grammar" [] []
instance XmlContent Grammar where
    toContents as =
        [CElem (Elem "Grammar" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["Grammar"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <Grammar>, "++)
instance XmlAttributes Grammar where
    fromAttrs as =
        Grammar
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

instance HTypeable Spelling where
    toHType x = Defined "Spelling" [] []
instance XmlContent Spelling where
    toContents Spelling =
        [CElem (Elem "Spelling" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["Spelling"]
        ; return Spelling
        } `adjustErr` ("in <Spelling>, "++)

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
