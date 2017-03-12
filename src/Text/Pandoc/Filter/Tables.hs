{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Text.Pandoc.Filter.Tables (
       formatTableBlock, makeTable
) where
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Filter.CSVTable
import Text.Pandoc.Filter.XMLTable
import Text.Pandoc
import qualified Data.List as DL
import qualified Data.Map as M
import Data.Maybe (fromMaybe)


-- Pandoc table functions ---------------------------------------------------------------
makeCell :: String -> TableCell
makeCell c = let (Pandoc _ bs) = handleError $ readMarkdown def c in
             bs

makeRow :: [String] -> [TableCell]
makeRow cells = map makeCell cells

makeHeader :: String -> TableCell
makeHeader c = [Plain [Strong [Str c]]]

makeAlign :: String -> Alignment
makeAlign "l" = AlignLeft
makeAlign "c" = AlignCenter
makeAlign "r" = AlignRight
makeAlign  _  = AlignDefault

makeTable :: String -> [String] -> [Double] -> [String] -> [[String]] -> Block
makeTable caption heads widths aligns rows = 
  (Table [(Str caption)] 
             (if null aligns then map (\_ -> AlignDefault) heads else map makeAlign aligns)
             (if null widths then map (\_ -> 0) heads else widths)
             (map makeHeader heads)
             (map makeRow rows))

makeItem :: [String] -> [Block]
makeItem cells = let i = DL.intercalate ": " cells in
                 makeCell i

makeList :: [[String]] -> Block
makeList rows = 
  let rows' = map makeItem rows in
  (OrderedList (1, DefaultStyle, DefaultDelim) rows')
-- END Pandoc table functions -----------------------------------------------------------

-- Pivot functions ----------------------------------------------------------------------

headOrPad :: a -> [a] -> a
headOrPad pad vs = 
    if DL.null vs then pad
    else DL.head vs

tailOrPad :: [a] -> [a]
tailOrPad l = if DL.null l then [] else  DL.tail l

notNull :: [a] -> Bool
notNull = not . DL.null
-- Take a list of lists of data in the same category, and make a table
-- with columns containing data of the same category.
transposePadWith ::  v -> [[v]] -> [[v]]
transposePadWith pad xs =
    if DL.null $ DL.filter notNull xs then []
    else let hs = DL.map (headOrPad pad) xs
         in hs:(transposePadWith pad $ DL.map tailOrPad xs)

-- Put the cell_col field of each row in a Map bucket with key
-- matching the pivot_col field of that row.
groupRows :: Int -> Int -> [[String]] -> [[String]]
groupRows pivot_col cell_col rows = 
   let m = DL.foldl' (\a r -> if length r > max pivot_col cell_col then  M.insertWith (++) (r !! pivot_col) [(r !! cell_col)] a else a) (M.empty :: M.Map String [String]) rows 
   in DL.filter notNull $ DL.map (\k -> fromMaybe [] $ M.lookup k m) $ DL.nub $ DL.map (getField pivot_col) rows

getField :: Int -> [String] -> String
getField col row = if length row > col  then row !! col else "(none)"

-- Turn input table into a rotated version where columns are values
-- from 'cell_field' grouped according to values in 'pivot_field'
pivotTable :: String -> String -> ([String], [[String]]) -> ([String], [[String]])
pivotTable pivot_field cell_field (heads, rows) = 
    -- Default is to pivot on the first column.
    let pivot_col = fromMaybe 0 $ DL.elemIndex pivot_field heads
    -- Default is to populate cells with the second column.
        cell_col = fromMaybe 1 $ DL.elemIndex cell_field heads
        rows' = groupRows pivot_col cell_col rows
        rows'' = transposePadWith "" rows'
        heads' = DL.nub $ DL.map (getField pivot_col) rows
    in (heads', rows'')

-- END Pivot functions ----------------------------------------------------------------------

pivotCSV :: String -> [String] -> [(String, String)] -> String -> IO Block
pivotCSV _ _ namevals contents = do
  let pivot_col = fromMaybe "State" $ lookup "pivot_col" namevals
      value_col = fromMaybe "Desc" $ lookup "value_col" namevals
      delim = head $ fromMaybe "," $ lookup "delim" namevals
      tbl = getCSV delim contents
      (heads, rows) = pivotTable pivot_col value_col tbl
  return $ makeTable (fromMaybe "Table" $ lookup "caption" namevals) 
            heads 
            ((read $ fromMaybe "[]"  $ lookup "widths" namevals)::[Double]) 
            (read $ fromMaybe "[]" $ lookup "align" namevals)
            rows

formatCSV _ _ namevals contents = do
  let delim = head $ fromMaybe "," $ lookup "delim" namevals
      (heads, rows) = getCSV delim contents
  makeTable (fromMaybe "Table" $ lookup "caption" namevals) 
            heads 
            ((read $ fromMaybe "[]"  $ lookup "widths" namevals)::[Double]) 
            (read $ fromMaybe "[]" $ lookup "align" namevals)
            rows

formatXML :: String -> [String] -> [(String, String)] -> String -> IO Block
formatXML _ classes namevals contents = do
  let cols = read $ fromMaybe "[]" $ lookup "columns" namevals
  let root = fromMaybe "" $ lookup "root" namevals
  let (heads, rows) = getXML root cols contents 
  if elem "table" classes then 
      let heads' = read $ fromMaybe "[]" $ lookup "headers" namevals
          widths = ((read $ fromMaybe "[]"  $ lookup "widths" namevals)::[Double])
          aligns = (read $ fromMaybe "[]" $ lookup "align" namevals)
          caption =  fromMaybe "Table" $ lookup "caption" namevals
      in return $ makeTable caption (if null heads'  then heads else heads') widths aligns rows
  else if elem "pivot" classes then
           let pivot_col = fromMaybe "State" $ lookup "pivot_col" namevals
               value_col = fromMaybe "Desc" $ lookup "value_col" namevals
               (heads', rows') = pivotTable pivot_col value_col (heads, rows)
               widths = ((read $ fromMaybe "[]"  $ lookup "widths" namevals)::[Double])
               aligns = (read $ fromMaybe "[]" $ lookup "align" namevals)
               caption =  fromMaybe "Table" $ lookup "caption" namevals
           in return $ makeTable caption heads' widths aligns rows'
  else
      return $ makeList rows


formatTableBlock :: Block -> IO Block
formatTableBlock cb@(CodeBlock (_id, classes, namevals) contents) 
  | elem "table" classes && elem "csv" classes = return $ formatCSV id classes namevals contents
  | elem "pivot" classes && elem "csv" classes = pivotCSV _id classes namevals contents
  | elem "table" classes && elem "xml" classes = formatXML _id classes namevals contents
  | elem "pivot" classes && elem "xml" classes = formatXML _id classes namevals contents
  | elem "list"  classes && elem "xml" classes = formatXML _id classes namevals contents
  | otherwise = return cb
formatTableBlock x = return x

