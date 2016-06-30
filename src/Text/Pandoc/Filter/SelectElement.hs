{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Filter.SelectElement (selectEl) where
--import Data.Maybe (maybe, fromJust, fromMaybe)
import Text.Pandoc

selectEl :: [String] -> [String] -> Block -> IO Block
selectEl sel_ids sel_classes el@(Div (_id, classes, _) _) = selectEl' sel_ids sel_classes el _id classes
selectEl _ _  el@(Header _ (_, [], _) _) = return el -- if class not specified, assume eq. to "always"
selectEl sel_ids sel_classes el@(Header _ (_id, classes, _) _) = selectEl' sel_ids sel_classes el _id classes
selectEl _ _ x = return x

selectEl' :: [String] -> [String] -> Block -> String -> [String] -> IO Block
selectEl' sel_ids sel_classes el _id classes = 
    return $ if elem _id sel_ids then el 
             else if any (\c -> elem c sel_classes) classes then el
                  else Null

