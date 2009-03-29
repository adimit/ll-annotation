-- |
-- Module : Annotator
-- Copyright : 2009 Aleksandar Dimitrov
-- License : BSD3
--
-- Maintainer : Aleksandar Dimitrov <aleks.dimitrov@gmail.com>
-- Stability : provisional
-- Portability : unportable
-- 
-- This module defines the graphical user interface for the Annotator.

module Annotator.Interface
       ( -- * GUI entry points  
       runGUI
       , runGUIWithFile 
       ) where

import Annotator.DTD
import Annotator.Interface.Constants
import Annotator.Interface.Types
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.List
import qualified Data.Map as M
import GHC.List hiding (span)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Windows.Dialog
import Text.XML.HaXml.XmlContent.Haskell (readXml)
import Text.XML.HaXml.XmlContent (fWriteXml)

-- Takes a corpus and returns a string representing the corpus' text in plain text.
xmlToTokenString :: Corpus -> (Int,String,TokenMap)
xmlToTokenString (Corpus (Tokens (Tokens_Attrs slen) xs) _) = foldr f ((read slen),"",M.empty) xs
        where f !token@(Token _ t) !(!i,!s,!m) = 
               let l = length t
                   i' = l `seq` (i-l)
               in  (i',t++s,M.insert (Span (i') l) token m)

-- Generic function to notify the user something bad has happened.
showError :: String -> IO ()
showError = putStrLn

-- Builds a GTK file Chooser to open files.
constructOpenFileChooser :: Gui -> IO FileChooserDialog
constructOpenFileChooser gui = do
    fc <- fileChooserDialogNew (Just "Open Corpus")
                               (Just (window gui))
                               FileChooserActionOpen
                               [("gtk-cancel",ResponseCancel),("gtk-open",ResponseAccept)]
    fileChooserSetSelectMultiple fc False
    ff <- xmlFileFilter
    fileChooserAddFilter fc ff
    return fc

-- Builds a GTK file filter to only allow XML documents.
xmlFileFilter :: IO FileFilter
xmlFileFilter = do ff <- fileFilterNew
                   fileFilterSetName ff "XML files"
                   fileFilterAddMimeType ff "text/xml"
                   return ff

findContext :: Gui -> EventM EButton Bool
findContext gui =
    do btn <- eventButton
       case btn of
           LeftButton -> do 
               coords <- eventCoordinates
               let (x,y) = truncCoordToInt coords
               liftIO $ do findToken gui (x,y)
                           return False
           _          -> return False
       
truncCoordToInt :: (Double,Double) -> (Int,Int)
truncCoordToInt (x,y) = (truncate x,truncate y)

findToken :: Gui -> (Int,Int) -> IO ()
findToken gui (x,y) = do
        bcrd <- (corpusView gui) `textViewWindowToBufferCoords` TextWindowWidget $ (x,y)
        iter <- uncurry (textViewGetIterAtLocation (corpusView gui)) bcrd
        loc <- textIterGetOffset iter
        Just tmap <- readIORef (tokens gui)
        let Just token = (Point loc) `M.lookup` tmap
        ref <- readIORef (selectedTkn gui)
        case ref of
             Nothing -> writeIORef (selectedTkn gui) (Just [token])
             Just toks -> if (not (elem token toks))
                               then writeIORef (selectedTkn gui) (Just $ toks ++ [token])
                               else writeIORef (selectedTkn gui) (Just $ delete token toks)
        putTokensOnLabel gui
                        
-- | Entry to the GUI
runGUI :: IO ()
runGUI = do gui <- prepareGUI 
            case gui of
                 Left s -> showError s
                 Right _ -> mainGUI

-- | Entry in to the GUI, whilst opening a corpus file.
runGUIWithFile :: FilePath -> IO ()
runGUIWithFile fn = do gui <-  prepareGUI
                       case gui of 
                            Left s -> showError s
                            Right g -> loadFile g fn >> mainGUI

-- Common code used by GUI entry points. Returns the GladeXML of the main GUI.
prepareGUI :: IO (Either String Gui)
prepareGUI = do
    initGUI
    mXml <- xmlNew gladeSource
    case mXml of
         Nothing       -> return $ Left ("Invalid glade xml file " ++ gladeSource)
         Just gladeXml -> do -- beware of ugliness ahead. This is a FIXME
                             w       <- xmlGetWidget gladeXml castToWindow windowMain
                             tl       <- xmlGetWidget gladeXml castToLabel "tokenLabel"
                             textView  <- xmlGetWidget gladeXml castToTextView "corpusView"
                             nothingRef <- newIORef Nothing
                             nothingRef' <- newIORef Nothing
                             nothingRef'' <- newIORef Nothing
                             nothingRef''' <- newIORef Nothing
                             let gui = Gui { corpusView  = textView
                                           , window      = w
                                           , xml         = gladeXml
                                           , tokenLabel  = tl
                                           , corpusClick = nothingRef
                                           , tokens      = nothingRef'
                                           , selectedTkn = nothingRef''
                                           , xmlDocument = nothingRef'''
                                           }
                             initControls gui
                             onDestroy w mainQuit
                             widgetShowAll w
                             return $ Right gui

-- Helper function to associate all controls with actions
initControls :: Gui -> IO ()
initControls gui = do quitItem <- xmlGetWidget (xml gui) castToMenuItem menuItemQuit
                      openItem <- xmlGetWidget (xml gui) castToMenuItem menuItemOpen
                      clearBtn <- xmlGetWidget (xml gui) castToButton "clearButton"
                      spellBtn <- xmlGetWidget (xml gui) castToButton "errorSpelButton"
                      quitItem `afterActivateLeaf` widgetDestroy (window gui)
                      openItem `afterActivateLeaf` openItemHandler gui
                      clearBtn `onClicked` clearBtnHandler gui
                      spellBtn `onClicked` spellBtnHandler gui
                      return ()

openItemHandler :: Gui -> IO ()
openItemHandler gui = do fn <- openFileAction gui
                         case fn of
                              (Just fn') -> loadFile gui fn'
                              _          -> return ()

saveItemHandler :: Gui -> FilePath -> IO ()
saveItemHandler gui fn = do ref <- readIORef (xmlDocument gui)
                            case ref of
                                 Just doc -> fWriteXml fn doc
                                 Nothing  -> showError "Load a file first"

saveAsItemHandler :: Gui -> IO ()
saveAsItemHandler = undefined

spellBtnHandler :: Gui -> IO ()
spellBtnHandler gui = do tks <- readIORef (selectedTkn gui)
                         case tks of
                              Nothing -> showError "Please select some tokens first!"
                              Just [] -> showError "Please select some tokens first!"
                              Just ts -> do addToErrors gui e
                                            clearBtnHandler gui
                                            where e = (Error (Errtoks $ unwords ids)
                                                             (TypeSpelling Spelling)
                                                             (Context $ unwords ctx)
                                                             Nothing
                                                             Nothing)
                                                  (ids,ctx) = tokenIdsAndContext ts
                           

clearBtnHandler :: Gui -> IO ()
clearBtnHandler gui =  do writeIORef (selectedTkn gui) (Just [])
                          putTokensOnLabel gui

tokenIdsAndContext :: [Token] -> ([String],[String])
tokenIdsAndContext = foldr tokenId ([],[])
           where tokenId :: Token -> ([String],[String]) -> ([String],[String])
                 tokenId (Token (Token_Attrs idx) ctx) (idxs,ctxs) = (idx:idxs,ctx:ctxs)

-- Queries the user for opening a file.
openFileAction :: Gui -> IO (Maybe FilePath)
openFileAction gui = do
    fc <- constructOpenFileChooser gui
    r  <- dialogRun fc
    widgetHide fc
    case r of
         ResponseAccept -> fileChooserGetFilename fc
         _              -> return Nothing

-- Load a corpus from a file, display it, and set the appropriate events.
loadFile :: Gui -> FilePath -> IO ()
loadFile gui fn = do
    tb <- textViewGetBuffer (corpusView gui)
    cntt <- readFile fn
    let corpus = readXml cntt
    case corpus of
         Left  s -> showError $ "XML Parsing failed: " ++ s
         Right c -> do
               updateRef (xmlDocument gui) c
               si <- xmlGetWidget (xml gui) castToMenuItem "menuItemSave"
               si `afterActivateLeaf` (saveItemHandler gui fn)
               widgetSetSensitive si True
               let (_,text,toks) = xmlToTokenString c
               tb `textBufferSetText` text
               connectId <- corpusView gui `on` buttonReleaseEvent $ findContext gui
               updateRef' (corpusClick gui) (maybeDisconnectOld connectId)
               updateRef (tokens gui) toks
                    where maybeDisconnectOld :: ConnectId TextView ->
                                                Maybe (ConnectId TextView) ->
                                                IO (ConnectId TextView)
                          maybeDisconnectOld cid payload =
                              do case payload of
                                   Just cidOld -> signalDisconnect cidOld
                                   Nothing     -> return ()
                                 return cid
    
putTokensOnLabel :: Gui -> IO ()
putTokensOnLabel gui = do ref <- readIORef (selectedTkn gui)
                          case ref of
                               (Just tkns) -> (tokenLabel gui) `labelSetText` (show (map r tkns))
                               Nothing     -> (tokenLabel gui) `labelSetText` ""
                               where  r :: Token -> String
                                      r (Token _ s) = s

updateRef  :: IORef (Maybe a) -> a -> IO ()
updateRef ref payload = updateRef' ref (const $ return payload)

updateRef' :: IORef (Maybe a) -> (Maybe a ->IO a) -> IO () 
updateRef' ref act = do var <- readIORef ref
                        a'  <- act var
                        writeIORef ref (Just a')

addToErrors :: Gui -> Error -> IO ()
addToErrors gui err = do ref <- readIORef (xmlDocument gui)
                         case ref of
                              Just doc -> writeIORef (xmlDocument gui) (Just $ (ate err) doc)
                              Nothing  -> return ()
                              where  ate e crp@(Corpus ts (Errors es)) =
                                            if e `elem` es
                                               then crp
                                               else Corpus ts (Errors (e:es))

removeFromErrors :: Gui -> Error -> IO ()
removeFromErrors gui err = do ref <- readIORef (xmlDocument gui)
                              case ref of
                                   Just doc -> writeIORef (xmlDocument gui) (Just $ (rfe err) doc)
                                   Nothing  -> return ()
                                   where  rfe e (Corpus ts (Errors es)) =
                                                 Corpus ts (Errors (delete e es))
