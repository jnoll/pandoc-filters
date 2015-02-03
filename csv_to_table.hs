{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--import Control.Applicative ((<$>), (<*>))
import CSVTable
import XMLTable
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.JSON
import Text.Pandoc.JSON
import System.IO
import System.IO.Error (IOError, try)
import Control.Monad.Trans (liftIO)
import Data.List (intercalate, null)
import Data.Csv hiding (lookup)
import Data.Char (ord)
import Data.Maybe (fromJust, fromMaybe)
import Text.XML.HXT.Core
import qualified Data.ByteString.Lazy.Char8            as BL

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
makeItem cells = let i = intercalate ": " cells in
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


format :: Block -> IO Block
format cb@(CodeBlock (id, classes, namevals) contents) 
  | elem "table" classes && elem "csv" classes = return $ formatCSV id classes namevals contents
  | elem "table" classes && elem "xml" classes = formatXML id classes namevals contents
  | elem "list" classes && elem "xml" classes = formatXML id classes namevals contents
  | otherwise = return cb
format x = return x

main :: IO ()
main = toJSONFilter format
