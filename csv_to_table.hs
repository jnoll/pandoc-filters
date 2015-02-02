{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--import Control.Applicative ((<$>), (<*>))
import XMLTable
import Text.Pandoc.JSON
import System.IO
import System.IO.Error (IOError, try)
import Control.Monad.Trans (liftIO)
import Data.List (intercalate)
import Data.Csv hiding (lookup)
import Data.Char (ord)
import Data.Vector as V
import Data.Maybe (fromJust, fromMaybe)
import Text.XML.HXT.Core
import qualified Data.ByteString.Lazy.Char8            as BL

decodeContents :: BL.ByteString ->  Either String (Vector (Vector String))
decodeContents c = decodeWith defaultDecodeOptions { decDelimiter = fromIntegral (ord '|') } NoHeader c

makeCell :: String -> TableCell
makeCell c = [(Plain [Str c])]

makeRow :: Vector String -> [TableCell]
makeRow r = toList $ V.map makeCell r

makeTable :: String -> [Double] -> Vector (Vector String) -> Block
makeTable caption widths c = let 
    hline = makeRow $ V.head c
    align = toList $ V.map (\_ -> AlignDefault) $ V.head c
--    widths = toList $ V.map (\_ -> 1.0/(fromIntegral $ V.length $ V.head c) ) $ V.head c
    widths' = if Prelude.length widths > 0 then widths else toList $ V.map (\_ -> 1.0/(fromIntegral $ V.length $ V.head c) ) $ V.head c
    clines = toList $ V.map makeRow $ V.tail c
    in (Table [(Str caption)] align widths' hline clines)


formatCSV id classes namevals contents = do
  case decodeContents $ BL.pack contents of
    Left s -> return $ Plain [Str s]
    Right d -> return $ makeTable (fromMaybe "Table" $ lookup "caption" namevals) ((read $ fromMaybe "[]"  $ lookup "widths" namevals)::[Double]) d

format :: Block -> IO Block
format cb@(CodeBlock (id, classes, namevals) contents) 
  | Prelude.elem "table" classes && Prelude.elem "csv" classes = formatCSV id classes namevals contents
  | Prelude.elem "table" classes && Prelude.elem "xml" classes = formatXML id classes namevals contents
  | Prelude.elem "list" classes && Prelude.elem "xml" classes = formatXML id classes namevals contents
  | otherwise = return cb
format x = return x

main :: IO ()
main = toJSONFilter format
