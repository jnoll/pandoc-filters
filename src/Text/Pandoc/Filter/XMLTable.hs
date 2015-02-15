module Text.Pandoc.Filter.XMLTable (getXML) where
import Text.XML.Light
import Data.Maybe (fromJust, fromMaybe)

defQname :: QName
defQname = QName {qName = "uh oh", qURI = Nothing, qPrefix = Nothing}

qname :: String -> QName
qname s = defQname { qName = s }

getField :: Element -> String -> String 
getField el field =
    strContent $ fromMaybe blank_element $ findChild (qname field) el

mkRow :: [String] -> Element -> [String]
mkRow cols el = map (getField el) cols

-- Retrieve a subtree and convert it into a table of rows.
getXML :: String -> [String]  -> String -> ([String], [[String]])
getXML root cols contents = 
  case parseXMLDoc contents of
    Just doc -> 
        let parent = fromMaybe blank_element $ findElement (qname root ) doc -- dfs for first matching element
            children = elChildren parent  -- get all children that are elements
        in (cols, map (mkRow cols) children)
    Nothing -> (["Error", "Details"], [["Unable to parse contents", contents]])