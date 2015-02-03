{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--import Control.Applicative ((<$>), (<*>))
module CSVTable (getCSV) where
import System.IO
import System.IO.Error (IOError, try)
import Control.Monad.Trans (liftIO)
import Data.List (intercalate)
import Data.Csv hiding (lookup)
import Data.Char (ord)
import Data.Vector as V
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.ByteString.Lazy.Char8            as BL

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
