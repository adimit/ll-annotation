module Annotator.DTD where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.OneOfN


{-Type decls-}

data Corpus = Corpus Tokens Errors
            deriving (Eq,Show)
newtype Errors = Errors [Record] 		deriving (Eq,Show)
data Tokens = Tokens Tokens_Attrs [Token]
            deriving (Eq,Show)
data Tokens_Attrs = Tokens_Attrs
    { tokensAmount :: String
    } deriving (Eq,Show)
data Record = Record Record_Attrs Errtoks Error (Maybe Target)
                     (Maybe Comment)
            deriving (Eq,Show)
data Record_Attrs = Record_Attrs
    { recordEcontext :: (Maybe String)
    , recordTransfer :: (Maybe Record_transfer)
    } deriving (Eq,Show)
data Record_transfer = Record_transfer_true
                     deriving (Eq,Show)
data Errtoks = Errtoks
    { errtoksIdx :: String
    } deriving (Eq,Show)
newtype Error = Error (Maybe Error_) 		deriving (Eq,Show)
data Error_ = Error_Context Context
            | Error_Grammar Grammar
            | Error_Spelling Spelling
            | Error_Gibberish Gibberish
            deriving (Eq,Show)
newtype Context = Context (Maybe Context_) 		deriving (Eq,Show)
data Context_ = Context_Lexical Lexical
              | Context_Idiom Idiom
              deriving (Eq,Show)
newtype Grammar = Grammar (Maybe Grammar_) 		deriving (Eq,Show)
data Grammar_ = Grammar_Agreement Agreement
              | Grammar_Omission Omission
              | Grammar_Wo Wo
              | Grammar_Morphology Morphology
              | Grammar_Redundancy Redundancy
              deriving (Eq,Show)
newtype Spelling = Spelling (Maybe Spelling_) 		deriving (Eq,Show)
data Spelling_ = Spelling_Homonymy Homonymy
               | Spelling_Register Register
               | Spelling_Diacritics Diacritics
               | Spelling_Local Local
               deriving (Eq,Show)
data Gibberish = Gibberish 		deriving (Eq,Show)
data Lexical = Lexical 		deriving (Eq,Show)
newtype Agreement = Agreement (Maybe Agreement_) 		deriving (Eq,Show)
data Agreement_ = Agreement_Number Number
                | Agreement_Gender Gender
                | Agreement_Case Case
                | Agreement_Person Person
                | Agreement_Tense Tense
                deriving (Eq,Show)
newtype Omission = Omission (Maybe Omission_) 		deriving (Eq,Show)
data Omission_ = Omission_Subject Subject
               | Omission_Predicate Predicate
               | Omission_Object Object
               | Omission_Article Article
               | Omission_Preposition Preposition
               deriving (Eq,Show)
newtype Object = Object (Maybe Object_) 		deriving (Eq,Show)
data Object_ = Object_Direct Direct
             | Object_Indirect Indirect
             deriving (Eq,Show)
newtype Morphology = Morphology (Maybe Morphology_) 		deriving (Eq,Show)
data Morphology_ = Morphology_Derivation Derivation
                 | Morphology_Compounding Compounding
                 | Morphology_Inflection Inflection
                 deriving (Eq,Show)
data Number = Number 		deriving (Eq,Show)
data Gender = Gender 		deriving (Eq,Show)
data Case = Case 		deriving (Eq,Show)
data Tense = Tense 		deriving (Eq,Show)
data Person = Person 		deriving (Eq,Show)
data Subject = Subject 		deriving (Eq,Show)
data Predicate = Predicate 		deriving (Eq,Show)
data Article = Article 		deriving (Eq,Show)
data Preposition = Preposition 		deriving (Eq,Show)
data Wo = Wo 		deriving (Eq,Show)
data Redundancy = Redundancy 		deriving (Eq,Show)
data Homonymy = Homonymy 		deriving (Eq,Show)
data Register = Register 		deriving (Eq,Show)
data Diacritics = Diacritics 		deriving (Eq,Show)
data Local = Local 		deriving (Eq,Show)
data Lexis = Lexis 		deriving (Eq,Show)
data Idiom = Idiom 		deriving (Eq,Show)
data Direct = Direct 		deriving (Eq,Show)
data Indirect = Indirect 		deriving (Eq,Show)
data Derivation = Derivation 		deriving (Eq,Show)
data Compounding = Compounding 		deriving (Eq,Show)
data Inflection = Inflection 		deriving (Eq,Show)
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
          { tokensAmount = definiteA fromAttrToStr "tokens" "amount" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "amount" (tokensAmount v)
        ]

instance HTypeable Record where
    toHType x = Defined "record" [] []
instance XmlContent Record where
    toContents (Record as a b c d) =
        [CElem (Elem "record" (toAttrs as) (toContents a ++ toContents b ++
                                            maybe [] toContents c ++ maybe [] toContents d)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["record"]
        ; interior e $ return (Record (fromAttrs as)) `apply` parseContents
                       `apply` parseContents `apply` optional parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <record>, "++)
instance XmlAttributes Record_Attrs where
    fromAttrs as =
        Record_Attrs
          { recordEcontext = possibleA fromAttrToStr "econtext" as
          , recordTransfer = possibleA fromAttrToTyp "transfer" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "econtext" (recordEcontext v)
        , maybeToAttr toAttrFrTyp "transfer" (recordTransfer v)
        ]

instance XmlAttrType Record_transfer where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "true" = Just Record_transfer_true
            translate _ = Nothing
    toAttrFrTyp n Record_transfer_true = Just (n, str2attr "true")

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
    toContents (Error a) =
        [CElem (Elem "error" [] (maybe [] toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["error"]
        ; interior e $ return (Error) `apply` optional parseContents
        } `adjustErr` ("in <error>, "++)

instance HTypeable Error_ where
    toHType x = Defined "error" [] []
instance XmlContent Error_ where
    toContents (Error_Context a) = toContents a
    toContents (Error_Grammar a) = toContents a
    toContents (Error_Spelling a) = toContents a
    toContents (Error_Gibberish a) = toContents a
    parseContents = oneOf
        [ return (Error_Context) `apply` parseContents
        , return (Error_Grammar) `apply` parseContents
        , return (Error_Spelling) `apply` parseContents
        , return (Error_Gibberish) `apply` parseContents
        ] `adjustErr` ("in <error>, "++)

instance HTypeable Context where
    toHType x = Defined "context" [] []
instance XmlContent Context where
    toContents (Context a) =
        [CElem (Elem "context" [] (maybe [] toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["context"]
        ; interior e $ return (Context) `apply` optional parseContents
        } `adjustErr` ("in <context>, "++)

instance HTypeable Context_ where
    toHType x = Defined "context" [] []
instance XmlContent Context_ where
    toContents (Context_Lexical a) = toContents a
    toContents (Context_Idiom a) = toContents a
    parseContents = oneOf
        [ return (Context_Lexical) `apply` parseContents
        , return (Context_Idiom) `apply` parseContents
        ] `adjustErr` ("in <context>, "++)

instance HTypeable Grammar where
    toHType x = Defined "grammar" [] []
instance XmlContent Grammar where
    toContents (Grammar a) =
        [CElem (Elem "grammar" [] (maybe [] toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["grammar"]
        ; interior e $ return (Grammar) `apply` optional parseContents
        } `adjustErr` ("in <grammar>, "++)

instance HTypeable Grammar_ where
    toHType x = Defined "grammar" [] []
instance XmlContent Grammar_ where
    toContents (Grammar_Agreement a) = toContents a
    toContents (Grammar_Omission a) = toContents a
    toContents (Grammar_Wo a) = toContents a
    toContents (Grammar_Morphology a) = toContents a
    toContents (Grammar_Redundancy a) = toContents a
    parseContents = oneOf
        [ return (Grammar_Agreement) `apply` parseContents
        , return (Grammar_Omission) `apply` parseContents
        , return (Grammar_Wo) `apply` parseContents
        , return (Grammar_Morphology) `apply` parseContents
        , return (Grammar_Redundancy) `apply` parseContents
        ] `adjustErr` ("in <grammar>, "++)

instance HTypeable Spelling where
    toHType x = Defined "spelling" [] []
instance XmlContent Spelling where
    toContents (Spelling a) =
        [CElem (Elem "spelling" [] (maybe [] toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["spelling"]
        ; interior e $ return (Spelling) `apply` optional parseContents
        } `adjustErr` ("in <spelling>, "++)

instance HTypeable Spelling_ where
    toHType x = Defined "spelling" [] []
instance XmlContent Spelling_ where
    toContents (Spelling_Homonymy a) = toContents a
    toContents (Spelling_Register a) = toContents a
    toContents (Spelling_Diacritics a) = toContents a
    toContents (Spelling_Local a) = toContents a
    parseContents = oneOf
        [ return (Spelling_Homonymy) `apply` parseContents
        , return (Spelling_Register) `apply` parseContents
        , return (Spelling_Diacritics) `apply` parseContents
        , return (Spelling_Local) `apply` parseContents
        ] `adjustErr` ("in <spelling>, "++)

instance HTypeable Gibberish where
    toHType x = Defined "gibberish" [] []
instance XmlContent Gibberish where
    toContents Gibberish =
        [CElem (Elem "gibberish" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["gibberish"]
        ; return Gibberish
        } `adjustErr` ("in <gibberish>, "++)

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
    toContents (Agreement a) =
        [CElem (Elem "agreement" [] (maybe [] toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["agreement"]
        ; interior e $ return (Agreement) `apply` optional parseContents
        } `adjustErr` ("in <agreement>, "++)

instance HTypeable Agreement_ where
    toHType x = Defined "agreement" [] []
instance XmlContent Agreement_ where
    toContents (Agreement_Number a) = toContents a
    toContents (Agreement_Gender a) = toContents a
    toContents (Agreement_Case a) = toContents a
    toContents (Agreement_Person a) = toContents a
    toContents (Agreement_Tense a) = toContents a
    parseContents = oneOf
        [ return (Agreement_Number) `apply` parseContents
        , return (Agreement_Gender) `apply` parseContents
        , return (Agreement_Case) `apply` parseContents
        , return (Agreement_Person) `apply` parseContents
        , return (Agreement_Tense) `apply` parseContents
        ] `adjustErr` ("in <agreement>, "++)

instance HTypeable Omission where
    toHType x = Defined "omission" [] []
instance XmlContent Omission where
    toContents (Omission a) =
        [CElem (Elem "omission" [] (maybe [] toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["omission"]
        ; interior e $ return (Omission) `apply` optional parseContents
        } `adjustErr` ("in <omission>, "++)

instance HTypeable Omission_ where
    toHType x = Defined "omission" [] []
instance XmlContent Omission_ where
    toContents (Omission_Subject a) = toContents a
    toContents (Omission_Predicate a) = toContents a
    toContents (Omission_Object a) = toContents a
    toContents (Omission_Article a) = toContents a
    toContents (Omission_Preposition a) = toContents a
    parseContents = oneOf
        [ return (Omission_Subject) `apply` parseContents
        , return (Omission_Predicate) `apply` parseContents
        , return (Omission_Object) `apply` parseContents
        , return (Omission_Article) `apply` parseContents
        , return (Omission_Preposition) `apply` parseContents
        ] `adjustErr` ("in <omission>, "++)

instance HTypeable Object where
    toHType x = Defined "object" [] []
instance XmlContent Object where
    toContents (Object a) =
        [CElem (Elem "object" [] (maybe [] toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["object"]
        ; interior e $ return (Object) `apply` optional parseContents
        } `adjustErr` ("in <object>, "++)

instance HTypeable Object_ where
    toHType x = Defined "object" [] []
instance XmlContent Object_ where
    toContents (Object_Direct a) = toContents a
    toContents (Object_Indirect a) = toContents a
    parseContents = oneOf
        [ return (Object_Direct) `apply` parseContents
        , return (Object_Indirect) `apply` parseContents
        ] `adjustErr` ("in <object>, "++)

instance HTypeable Morphology where
    toHType x = Defined "morphology" [] []
instance XmlContent Morphology where
    toContents (Morphology a) =
        [CElem (Elem "morphology" [] (maybe [] toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["morphology"]
        ; interior e $ return (Morphology) `apply` optional parseContents
        } `adjustErr` ("in <morphology>, "++)

instance HTypeable Morphology_ where
    toHType x = Defined "morphology" [] []
instance XmlContent Morphology_ where
    toContents (Morphology_Derivation a) = toContents a
    toContents (Morphology_Compounding a) = toContents a
    toContents (Morphology_Inflection a) = toContents a
    parseContents = oneOf
        [ return (Morphology_Derivation) `apply` parseContents
        , return (Morphology_Compounding) `apply` parseContents
        , return (Morphology_Inflection) `apply` parseContents
        ] `adjustErr` ("in <morphology>, "++)

instance HTypeable Number where
    toHType x = Defined "number" [] []
instance XmlContent Number where
    toContents Number =
        [CElem (Elem "number" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["number"]
        ; return Number
        } `adjustErr` ("in <number>, "++)

instance HTypeable Gender where
    toHType x = Defined "gender" [] []
instance XmlContent Gender where
    toContents Gender =
        [CElem (Elem "gender" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["gender"]
        ; return Gender
        } `adjustErr` ("in <gender>, "++)

instance HTypeable Case where
    toHType x = Defined "case" [] []
instance XmlContent Case where
    toContents Case =
        [CElem (Elem "case" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["case"]
        ; return Case
        } `adjustErr` ("in <case>, "++)

instance HTypeable Tense where
    toHType x = Defined "tense" [] []
instance XmlContent Tense where
    toContents Tense =
        [CElem (Elem "tense" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["tense"]
        ; return Tense
        } `adjustErr` ("in <tense>, "++)

instance HTypeable Person where
    toHType x = Defined "person" [] []
instance XmlContent Person where
    toContents Person =
        [CElem (Elem "person" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["person"]
        ; return Person
        } `adjustErr` ("in <person>, "++)

instance HTypeable Subject where
    toHType x = Defined "subject" [] []
instance XmlContent Subject where
    toContents Subject =
        [CElem (Elem "subject" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["subject"]
        ; return Subject
        } `adjustErr` ("in <subject>, "++)

instance HTypeable Predicate where
    toHType x = Defined "predicate" [] []
instance XmlContent Predicate where
    toContents Predicate =
        [CElem (Elem "predicate" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["predicate"]
        ; return Predicate
        } `adjustErr` ("in <predicate>, "++)

instance HTypeable Article where
    toHType x = Defined "article" [] []
instance XmlContent Article where
    toContents Article =
        [CElem (Elem "article" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["article"]
        ; return Article
        } `adjustErr` ("in <article>, "++)

instance HTypeable Preposition where
    toHType x = Defined "preposition" [] []
instance XmlContent Preposition where
    toContents Preposition =
        [CElem (Elem "preposition" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["preposition"]
        ; return Preposition
        } `adjustErr` ("in <preposition>, "++)

instance HTypeable Wo where
    toHType x = Defined "wo" [] []
instance XmlContent Wo where
    toContents Wo =
        [CElem (Elem "wo" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["wo"]
        ; return Wo
        } `adjustErr` ("in <wo>, "++)

instance HTypeable Redundancy where
    toHType x = Defined "redundancy" [] []
instance XmlContent Redundancy where
    toContents Redundancy =
        [CElem (Elem "redundancy" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["redundancy"]
        ; return Redundancy
        } `adjustErr` ("in <redundancy>, "++)

instance HTypeable Homonymy where
    toHType x = Defined "homonymy" [] []
instance XmlContent Homonymy where
    toContents Homonymy =
        [CElem (Elem "homonymy" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["homonymy"]
        ; return Homonymy
        } `adjustErr` ("in <homonymy>, "++)

instance HTypeable Register where
    toHType x = Defined "register" [] []
instance XmlContent Register where
    toContents Register =
        [CElem (Elem "register" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["register"]
        ; return Register
        } `adjustErr` ("in <register>, "++)

instance HTypeable Diacritics where
    toHType x = Defined "diacritics" [] []
instance XmlContent Diacritics where
    toContents Diacritics =
        [CElem (Elem "diacritics" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["diacritics"]
        ; return Diacritics
        } `adjustErr` ("in <diacritics>, "++)

instance HTypeable Local where
    toHType x = Defined "local" [] []
instance XmlContent Local where
    toContents Local =
        [CElem (Elem "local" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["local"]
        ; return Local
        } `adjustErr` ("in <local>, "++)

instance HTypeable Lexis where
    toHType x = Defined "lexis" [] []
instance XmlContent Lexis where
    toContents Lexis =
        [CElem (Elem "lexis" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["lexis"]
        ; return Lexis
        } `adjustErr` ("in <lexis>, "++)

instance HTypeable Idiom where
    toHType x = Defined "idiom" [] []
instance XmlContent Idiom where
    toContents Idiom =
        [CElem (Elem "idiom" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["idiom"]
        ; return Idiom
        } `adjustErr` ("in <idiom>, "++)

instance HTypeable Direct where
    toHType x = Defined "direct" [] []
instance XmlContent Direct where
    toContents Direct =
        [CElem (Elem "direct" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["direct"]
        ; return Direct
        } `adjustErr` ("in <direct>, "++)

instance HTypeable Indirect where
    toHType x = Defined "indirect" [] []
instance XmlContent Indirect where
    toContents Indirect =
        [CElem (Elem "indirect" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["indirect"]
        ; return Indirect
        } `adjustErr` ("in <indirect>, "++)

instance HTypeable Derivation where
    toHType x = Defined "derivation" [] []
instance XmlContent Derivation where
    toContents Derivation =
        [CElem (Elem "derivation" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["derivation"]
        ; return Derivation
        } `adjustErr` ("in <derivation>, "++)

instance HTypeable Compounding where
    toHType x = Defined "compounding" [] []
instance XmlContent Compounding where
    toContents Compounding =
        [CElem (Elem "compounding" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["compounding"]
        ; return Compounding
        } `adjustErr` ("in <compounding>, "++)

instance HTypeable Inflection where
    toHType x = Defined "inflection" [] []
instance XmlContent Inflection where
    toContents Inflection =
        [CElem (Elem "inflection" [] []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["inflection"]
        ; return Inflection
        } `adjustErr` ("in <inflection>, "++)

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
