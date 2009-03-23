module Annotator.Interface ( runGUI, runGUIWithFile ) where

import Annotator.DTD
import Annotator.Interface.Constants
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Windows.Dialog
import Text.XML.HaXml.XmlContent.Haskell (readXml)

-- helper function to associate all menu items with actions
initControls :: GladeXML -> IO ()
initControls xml = do quitItem <- xmlGetWidget xml castToMenuItem menuItemQuit
                      openItem <- xmlGetWidget xml castToMenuItem menuItemOpen
                      window   <- xmlGetWidget xml castToWindow windowMain
                      quitItem `afterActivateLeaf` do widgetDestroy window
                      openItem `afterActivateLeaf` openFileAction window xml
                      return ()

openFileAction :: Window -> GladeXML -> IO ()
openFileAction window xml = do
    fc <- constructOpenFileChooser window
    r  <- dialogRun fc
    case r of
         ResponseAccept -> do fn <- fileChooserGetFilename fc
                              case fn of
                                   Just fn' -> loadFile xml fn'
                                   Nothing  -> putStr ""
         _ ->putStr "" -- any better way of no-op in IO ()?
    widgetHide fc

loadFile :: GladeXML -> String -> IO ()
loadFile xml fn = do putStrLn $ "Opening File: " ++ fn
                     ta <- xmlGetWidget xml castToTextView corpusView
                     tb <- textViewGetBuffer ta
                     content <- readFile fn
                     let corpus = readXml content
                     case corpus of
                          Left  s -> showError s
                          Right s -> do tb `textBufferSetText` (xmlToTokenString s)

xmlToTokenString :: Corpus -> String
xmlToTokenString (Corpus (Tokens xs) _)= tokenListToString xs

tokenListToString :: [Token] -> String
tokenListToString = concat . map tokenToString
    where tokenToString (Token _ s) = s

showError = undefined

constructOpenFileChooser :: Window -> IO FileChooserDialog
constructOpenFileChooser w = do 
    fc <- fileChooserDialogNew (Just "Open Corpus")
                               (Just w)
                               FileChooserActionOpen
                               [("gtk-cancel",ResponseCancel),("gtk-open",ResponseAccept)]
    fileChooserSetSelectMultiple fc False
    ff <- xmlFileFilter
    fileChooserAddFilter fc ff
    return fc

xmlFileFilter :: IO FileFilter
xmlFileFilter = do ff <- fileFilterNew
                   fileFilterSetName ff "XML files"
                   fileFilterAddMimeType ff "text/xml"
                   return ff

runGUI :: IO ()
runGUI = do prepareGUI
            mainGUI

runGUIWithFile :: String -> IO ()
runGUIWithFile fn = do xml <-  prepareGUI
                       loadFile xml fn
                       mainGUI

prepareGUI :: IO GladeXML
prepareGUI = do initGUI
                Just xml <- xmlNew gladeSource
                window <- xmlGetWidget xml castToWindow windowMain
                initControls xml
                onDestroy window mainQuit
                widgetShowAll window
                return xml
