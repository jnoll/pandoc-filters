{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--import Control.Applicative ((<$>), (<*>))
module Text.Pandoc.Filter.CSVTable (getCSV) where
import qualified Data.List as DL
import Text.CSV.Lazy.String


-- convert DSV contents into "table" consisting of a header line and a
-- list of rows.  csvTableFull converts to list of String, with
-- invalid rows "repaired with padding" (according to haddoc).
getCSV :: Char -> String -> ([String], [[String]])
getCSV delim contents = let (head:rows) = fromCSVTable $ csvTableFull $ parseDSV False delim contents in
                        (head, rows)

