{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--import Control.Applicative ((<$>), (<*>))
module Text.Pandoc.Filter.CSVTable (getCSV, getCSVPivot) where
import System.IO
import System.IO.Error (IOError, try)
import qualified Data.List as DL
import Data.Csv hiding (lookup)
import Data.CSV.Conduit
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List as CL
import Data.Char (ord)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Char8 as BSC

decodeContents :: BL.ByteString ->  Either String (Vector (Vector String))
decodeContents c = decodeWith defaultDecodeOptions { decDelimiter = fromIntegral (ord '|') } NoHeader c

makeRow :: Vector String -> [String]
makeRow r = V.toList  r

getCSV :: String -> ([String], [[String]])
getCSV contents = case decodeContents $ BL.pack contents of
    Left s -> (["error"], [[s]])
    Right d -> let heads = V.toList $  V.head d
                   rows = V.toList $ V.map makeRow $ V.tail d
               in (heads, rows)

headOrPad :: a -> [a] -> a
headOrPad pad e = 
    if DL.null e then pad
    else DL.head e

tailOrPad :: [a] -> [a] 
tailOrPad l = if DL.null l then [] else DL.tail l

transposePadWith :: (a -> b) -> a -> [[a]] -> [[b]]
transposePadWith xfrm pad xs =
    if DL.null $ DL.filter (not . DL.null) xs then []
    else let hs = DL.map (headOrPad pad) xs in
         (DL.map xfrm hs):(transposePadWith xfrm pad $ DL.map tailOrPad xs)

groupRows :: Monad m => BS.ByteString  ->  Conduit (MapRow BS.ByteString) m [MapRow BS.ByteString]
groupRows pivot_col = CL.groupBy (\f s -> (fromJust $ M.lookup pivot_col f) == (fromJust $ M.lookup pivot_col s))

getField :: BS.ByteString -> MapRow BS.ByteString -> String
getField col row = BSC.unpack $ fromMaybe "(none)" $ M.lookup col row

getCSVPivot :: String -> String -> String -> IO ([String], [[String]])
getCSVPivot pivot_col cell_col contents = do
  r <- runResourceT 
       $ (sourceLbs $ BL.pack contents)
       $= intoCSV defCSVSettings  {csvSep = ','} 
       $= groupRows (BSC.pack pivot_col)
       $$ consume
  let rows = transposePadWith (getField (BSC.pack cell_col)) (M.empty :: MapRow BS.ByteString) r
  let heads = DL.nub $ DL.map (getField (BSC.pack pivot_col)) $ DL.map DL.head r
  return (heads, rows)