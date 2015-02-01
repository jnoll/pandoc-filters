{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.XML.HXT.Core
import Text.Pandoc.JSON
import Data.Maybe (fromJust, fromMaybe)
import BlockContents

makeCell :: String -> TableCell
makeCell c = [(Plain [Str c])]

getCell :: ArrowXml a => String ->  a XmlTree TableCell
getCell tag = deep (isElem >>> hasName tag) >>>
              proc el -> do
                 c <- getText <<< getChildren -< el
                 returnA -< makeCell c
      
getRow :: ArrowXml a => String -> [String] ->  a XmlTree [TableCell]
getRow t (col:cols) = deep (isElem >>> hasName t) >>>
    proc el -> do
      c <- getCell col -<  el 
      r <- getRow t cols -< el
      returnA -< c:r
getRow _ [] = proc el -> do returnA -< []

getRows :: ArrowXml a => String -> String -> [String] ->  a XmlTree [[TableCell]]
getRows root tag cols = 
    deep (isElem >>> hasName root) >>> 
         proc el -> do
           rs <- listA (getRow tag cols) -< el
           returnA -< rs
         
        
    
xmlTable :: String -> [String] -> [(String, String)] -> String -> IO Block
xmlTable id classes namevals contents = do
  [rows] <- runX (readString [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                           ] contents >>> getRows "features" "feature" ["id", "desc", "release"])
  return $ (Table [(Str $ fromMaybe "Table" $ lookup "caption" namevals)] 
                  [AlignDefault, AlignDefault, AlignDefault] 
                  ((read $ fromMaybe "[]"  $ lookup "widths" namevals)::[Double])
                  [makeCell "id", makeCell "desc", makeCell "release"] 
                  rows)


doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) = do
  (namevals', contents') <- getBlockContentsString namevals contents
  tbl <- xmlTable id classes namevals' contents'
  return tbl
doInclude x = return x

main :: IO ()
main = toJSONFilter doInclude
