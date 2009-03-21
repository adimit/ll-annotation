module Annotator.Interface ( runGUI ) where
     
import Annotator.Tokenizer
import Annotator.DTD
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
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
initMenu xml = do quitItem <- xmlGetWidget xml castToMenuItem "menuItemQuit"
                  window <- xmlGetWidget xml castToWindow "mainWindow"
                  afterActivateLeaf quitItem $ do widgetDestroy window

runGUI :: IO ()
runGUI = do initGUI
            Just xml <- xmlNew "gui/AnnotatorGUI.glade"
            window <- xmlGetWidget xml castToWindow "mainWindow"
            initMenu xml
            onDestroy window mainQuit
            widgetShowAll window
            mainGUI