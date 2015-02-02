{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.List (intercalate)
import Text.XML.HXT.Core
import Text.Pandoc.JSON
import Data.Maybe (fromJust, fromMaybe)
import BlockContents

makeCell :: String -> String
makeCell c = c

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
           rs <- listA (getRow tag cols makeCell) -< el
           returnA -< rs
        
    
xmlTable :: String -> IO [[String]]
xmlTable contents = do
  [rows] <- runX (readString [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                           ] contents >>> getRows "features" "feature" ["id", "desc", "release"])
  return rows


printRow :: [String] -> String
printRow cells = intercalate ": "  cells

toList :: [[String]] -> [String]
toList rows = map printRow rows

main :: IO ()
main = do
  contents <- getContents
  rows <- xmlTable contents
  putStrLn $ intercalate "\n" $ toList rows 
