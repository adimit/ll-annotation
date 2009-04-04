module Annotator.Interface.Util where

import Annotator.DTD
import Annotator.Interface.Types
import Control.Monad.Trans (liftIO)
import Control.Monad (forM,zipWithM_)
import Data.IORef
import Data.List
import qualified Data.Map as M
import GHC.List hiding (span)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import qualified Graphics.UI.Gtk.Gdk.Events as Old
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Windows.Dialog
import Text.XML.HaXml.XmlContent.Haskell (readXml)

import Data.Array

putTokensOnLabel :: Gui -> Label -> IO ()
putTokensOnLabel gui l = do tkns <- readIORef (selectedTkn gui)
                            l `labelSetText` (show $ map tokenString (sort tkns))

tokenId :: Token -> String
tokenId (Token (Token_Attrs idx) _) = idx

-- Helper for maps. Factored out due to common use.
tokenString :: Token -> String
tokenString (Token _ s) = s

updateRef  :: IORef (Maybe a) -> a -> IO ()
updateRef ref payload = updateRef' ref (const $ return payload)

updateRef' :: IORef (Maybe a) -> (Maybe a -> IO a) -> IO ()
updateRef' ref act = do var <- readIORef ref
                        a'  <- act var
                        writeIORef ref (Just a')

addToErrors :: Gui -> Record -> IO ()
addToErrors gui err = do ref <- readIORef (xmlDocument gui)
                         case ref of
                              Just doc -> writeIORef (xmlDocument gui) (Just $ (ate err) doc)
                              Nothing  -> return ()
                              where  ate e crp@(Corpus ts (Errors es)) =
                                            if e `elem` es
                                               then crp
                                               else Corpus ts (Errors (e:es))

removeFromErrors :: Gui -> Record -> IO ()
removeFromErrors gui err = do ref <- readIORef (xmlDocument gui)
                              case ref of
                                   Just doc -> writeIORef (xmlDocument gui) (Just $ (rfe err) doc)
                                   Nothing  -> return ()
                                   where  rfe e (Corpus ts (Errors es)) =
                                                 Corpus ts (Errors (delete e es))

-- Generic function to notify the user something bad has happened.
showError :: String -> IO ()
showError = putStrLn

xmlToArray :: Corpus -> Array Int Token
xmlToArray (Corpus (Tokens (Tokens_Attrs amount) ts) _) =
        array (0,amount') [(i,t) | (i,t) <-map f ts]
        where amount' = (read amount) - 1
              f t@(Token (Token_Attrs idx) _) = (read . (drop 1) $ idx,t)
