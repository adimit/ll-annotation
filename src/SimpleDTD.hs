module SimpleDTD where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.OneOfN


{-Type decls-}

data Corpus = Corpus Tokens Errors
            deriving (Eq,Show)
newtype Tokens = Tokens [Token] 		deriving (Eq,Show)
newtype Errors = Errors [Error] 		deriving (Eq,Show)
data Token = Token Token_Attrs String
           deriving (Eq,Show)
data Token_Attrs = Token_Attrs
    { tokenIdx :: String
    } deriving (Eq,Show)
newtype Error = Error Errortokens 		deriving (Eq,Show)
data Errortokens = Errortokens
    { errortokensIdx :: String
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

instance HTypeable Tokens where
    toHType x = Defined "tokens" [] []
instance XmlContent Tokens where
    toContents (Tokens a) =
        [CElem (Elem "tokens" [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["tokens"]
        ; interior e $ return (Tokens) `apply` many parseContents
        } `adjustErr` ("in <tokens>, "++)

instance HTypeable Errors where
    toHType x = Defined "errors" [] []
instance XmlContent Errors where
    toContents (Errors a) =
        [CElem (Elem "errors" [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["errors"]
        ; interior e $ return (Errors) `apply` many parseContents
        } `adjustErr` ("in <errors>, "++)

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

instance HTypeable Error where
    toHType x = Defined "error" [] []
instance XmlContent Error where
    toContents (Error a) =
        [CElem (Elem "error" [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["error"]
        ; interior e $ return (Error) `apply` parseContents
        } `adjustErr` ("in <error>, "++)

instance HTypeable Errortokens where
    toHType x = Defined "errortokens" [] []
instance XmlContent Errortokens where
    toContents as =
        [CElem (Elem "errortokens" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["errortokens"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <errortokens>, "++)
instance XmlAttributes Errortokens where
    fromAttrs as =
        Errortokens
          { errortokensIdx = definiteA fromAttrToStr "errortokens" "idx" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "idx" (errortokensIdx v)
        ]



{-Done-}
