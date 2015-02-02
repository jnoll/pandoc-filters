{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module XMLTable (formatXML) where
import Text.XML.HXT.Core
import Text.Pandoc.JSON
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import BlockContents

makeCell :: String -> TableCell
makeCell c = [(Plain [Str c])]

makeString :: String -> String
makeString c = c

getCell :: ArrowXml a => String -> (String -> b) ->  a XmlTree b
getCell tag mkcell = deep (isElem >>> hasName tag) >>>
              proc el -> do
                 c <- getText <<< getChildren -< el
                 returnA -< mkcell c
      
getRow :: ArrowXml a => String -> [String] -> (String -> b) -> a XmlTree [b]
getRow t (col:cols) mkcell = deep (isElem >>> hasName t) >>>
    proc el -> do
      c <- getCell col mkcell  -<  el 
      r <- getRow t cols mkcell -< el
      returnA -< c:r
getRow _ [] _ = proc el -> do returnA -< []

getRows :: ArrowXml a => String -> String -> [String] ->  a XmlTree [[String]]
getRows root tag cols = 
    deep (isElem >>> hasName root) >>> 
         proc el -> do
           rs <- listA (getRow tag cols makeString) -< el
           returnA -< rs
         
        
getXML :: String -> String -> [String]  -> String -> IO [[String]]
getXML root child cols contents = do
  [rows] <- runX (readString [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                           ] contents >>> getRows root child cols) -- features and feature should come from namevals
  return rows

makeRow :: [String] -> [TableCell]
makeRow cells = map makeCell cells
    
xmlTable :: String -> String -> [String] -> [Double] -> String -> String -> IO Block
xmlTable root child cols widths caption contents = do
  rows <- getXML root child cols contents
  let rows' = map makeRow rows
  return $ (Table [(Str caption)] 
                  [AlignDefault, AlignDefault, AlignDefault] 
                  widths
                  (map makeCell cols)
                  rows')

makeItem :: [String] -> [Block]
makeItem cells = let i = intercalate ": " cells in
                 [Plain [(Str i)]]
                     
 
xmlList :: String -> String -> [String] -> String -> IO Block
xmlList root child cols contents = do
  rows <- getXML root child cols contents
  let rows' = map makeItem rows
  return $ (OrderedList (1, DefaultStyle, DefaultDelim) rows')


formatXML :: String -> [String] -> [(String, String)] -> String -> IO Block
formatXML id classes namevals contents = do
  let cols = read $ fromMaybe "[]" $ lookup "columns" namevals
  let root = fromMaybe "" $ lookup "root" namevals
  let child = fromMaybe "" $ lookup "child" namevals
  if Prelude.elem "table" classes then 
      let widths = ((read $ fromMaybe "[]"  $ lookup "widths" namevals)::[Double])
          caption =  fromMaybe "Table" $ lookup "caption" namevals in
      xmlTable root child cols widths caption contents
  else
      xmlList root child cols contents
