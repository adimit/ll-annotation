module Annotator.Interface ( runGUI ) where
     
import Annotator.Tokenizer
import Annotator.DTD
import Annotator.Interface.Constants
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Windows.Dialog
import System.Environment (getArgs)
import Text.PrettyPrint.HughesPJ (render)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml.XmlContent (toXml)
import qualified Data.ByteString.Lazy.Char8 as C

-- Takes a set of tokens as Strings and transforms them to an XML document
createXML :: [String] -> Document ()
createXML xs = toXml False (Corpus tokens (Errors []))
        where tokens = mkTokens xs

-- Take tokenized text and turn it into a <tokens/> element
mkTokens :: [String] -> Tokens
mkTokens = Tokens . gentoks 0
         where gentoks :: Int -> [String] -> [Token]
               gentoks _ [] = []
               gentoks x (t:ts) = (Token attrs t) : gentoks (x+1) ts
                       where attrs = Token_Attrs { tokenIdx = "t" ++ show x }

-- render an XML document
prettyXML ::  Document () -> String
prettyXML = render . document 
                        
-- helper function to associate all menu items with actions
initControls :: GladeXML -> IO ()
initControls xml = do quitItem <- xmlGetWidget xml castToMenuItem menuItemQuit
                      openItem <- xmlGetWidget xml castToMenuItem menuItemOpen
                      window   <- xmlGetWidget xml castToWindow windowMain
                      quitItem `afterActivateLeaf` do widgetDestroy window
                      openItem `afterActivateLeaf` do fc <- constructOpenFileChooser window
                                                      r <- dialogRun fc
                                                      case r of
                                                        ResponseAccept -> do 
                                                          Just fn <- fileChooserGetFilename fc
                                                          loadFile xml fn
                                                        _              -> putStrLn "Cancelled"
                                                      widgetHide fc
                      return ()

loadFile :: GladeXML -> String -> IO ()
loadFile xml fn = do putStrLn $ "Opening File: " ++ fn
                     ta <- xmlGetWidget xml castToTextView corpusView
                     tb <- textViewGetBuffer ta
                     content <- readFile fn
                     textBufferSetText tb content




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
       
runGUI :: IO ()
runGUI = do initGUI
            Just xml <- xmlNew gladeSource
            window <- xmlGetWidget xml castToWindow windowMain
            initControls xml
            onDestroy window mainQuit
            widgetShowAll window
            mainGUI

xmlFileFilter :: IO FileFilter
xmlFileFilter = do ff <- fileFilterNew
                   fileFilterSetName ff "XML files"
                   fileFilterAddMimeType ff "text/xml"
                   return ff