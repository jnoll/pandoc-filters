{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Filter.XMLTable (getXML) where
import Text.XML.HXT.Core

makeString :: String -> String
makeString c = c

getCell :: ArrowXml a => String -> (String -> b) ->  a XmlTree b
getCell tag mkcell = deep (isElem >>> hasName tag) >>>
              proc el -> do
                 c <- getText <<< getChildren -< el
                 returnA -< mkcell c
      
getRow :: ArrowXml a => [String] -> (String -> b) -> a XmlTree [b]
--getRow t (col:cols) mkcell = deep (isElem >>> hasName t) >>>
getRow (col:cols) mkcell = 
    proc el -> do
      c <- getCell col mkcell -< el 
      r <- getRow cols mkcell -< el
      returnA -< c:r
getRow [] _ = proc el -> do returnA -< []

getRows :: ArrowXml a => String -> [String] ->  a XmlTree [[String]]
getRows root cols = 
    deep (isElem >>> hasName root) >>> 
         proc el -> do
           rs <- listA (getChildren >>> isElem >>> getRow cols makeString)  -< el
           returnA -< rs
         
        
getXML :: String -> [String]  -> String -> IO [[String]]
getXML root cols contents = do
  [rows] <- runX (readString [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                           ] contents >>> getRows root cols) -- features and feature should come from namevals
  return rows

 


