{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Text.Pandoc.Filter.Tables (
       formatTableBlock
) where
import Text.Pandoc.Filter.CSVTable
import Text.Pandoc.Filter.XMLTable
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc
import System.IO
import qualified  Data.List as DL
import Data.Csv hiding (lookup)
--import Data.Char (ord)
import Data.Maybe (fromJust, fromMaybe)
import Text.XML.HXT.Core


makeCell :: String -> TableCell
makeCell c = let (Pandoc _ bs) = readMarkdown def c in
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



formatCSV id classes namevals contents = do
  let (heads, rows) = getCSV contents
  makeTable (fromMaybe "Table" $ lookup "caption" namevals) 
            heads 
            ((read $ fromMaybe "[]"  $ lookup "widths" namevals)::[Double]) 
            (read $ fromMaybe "[]" $ lookup "align" namevals)
            rows

pivotCSV :: String -> [String] -> [(String, String)] -> String -> IO Block
pivotCSV id classes namevals contents = do
  let pivot_col = fromMaybe "State" $ lookup "pivot_col" namevals
  let value_col = fromMaybe "Desc" $ lookup "value_col" namevals
  (heads, rows) <- getCSVPivot pivot_col value_col contents
  return $ makeTable (fromMaybe "Table" $ lookup "caption" namevals) 
            heads 
            ((read $ fromMaybe "[]"  $ lookup "widths" namevals)::[Double]) 
            (read $ fromMaybe "[]" $ lookup "align" namevals)
            rows

formatXML :: String -> [String] -> [(String, String)] -> String -> IO Block
formatXML id classes namevals contents = do
  let cols = read $ fromMaybe "[]" $ lookup "columns" namevals
  let root = fromMaybe "" $ lookup "root" namevals
  rows <- getXML root cols contents 
  if elem "table" classes then 
      let heads = read $ fromMaybe "[]" $ lookup "headers" namevals
          widths = ((read $ fromMaybe "[]"  $ lookup "widths" namevals)::[Double])
          aligns = (read $ fromMaybe "[]" $ lookup "align" namevals)
          caption =  fromMaybe "Table" $ lookup "caption" namevals in
      return $ makeTable caption (if null heads  then cols else heads) widths aligns rows
  else
      return $ makeList rows


formatTableBlock :: Block -> IO Block
formatTableBlock cb@(CodeBlock (id, classes, namevals) contents) 
  | elem "table" classes && elem "csv" classes = return $ formatCSV id classes namevals contents
  | elem "pivot" classes && elem "csv" classes = pivotCSV id classes namevals contents
  | elem "table" classes && elem "xml" classes = formatXML id classes namevals contents
  | elem "list" classes && elem "xml" classes = formatXML id classes namevals contents
  | otherwise = return cb
formatTableBlock x = return x

