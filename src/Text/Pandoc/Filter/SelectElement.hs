{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Filter.SelectElement (selectAll, Options(..)) where

import Data.Maybe (fromMaybe, fromJust)
import Data.Time.Format (buildTime, parseTime, defaultTimeLocale)
import Data.Time.Calendar (Day, fromGregorian, diffDays)
import Data.Time.LocalTime (localDay)
import Text.Pandoc
import System.Console.CmdArgs

data Options = Options {
      opt_id    :: [String]  
    , opt_classes :: [String]
    , opt_color :: String       -- Make selected elements this color.
    , opt_date  :: String
    , opt_newer :: String
    , opt_older :: String
    , opt_includeEmpty :: Bool
} deriving (Data, Typeable, Show)


selectAll :: Options -> Pandoc -> Pandoc
selectAll opts = (bottomUp (selectInline opts)) . (bottomUp (selectBlock opts))
              
selectInline :: Options -> Inline -> Inline
selectInline opts sp@(Span (_id, classes, attrs) _) = 
    if selectEl opts _id classes attrs then colorSpan opts sp
    else if (not $ null $ opt_color opts) && (not $ elem "hidden" classes) then sp else  Str ""
selectInline _ x = x

selectBlock :: Options -> Block -> Block
selectBlock opts el@(Div (_id, classes, attrs) _) = selectBlock' opts el _id classes attrs
--selectBlock opts el@(Header _ (_, [], _) _) = el -- if class not specified, assume eq. to "always"
selectBlock opts el@(Header _ (_id, classes, attrs) _) = selectBlock' opts el _id classes attrs
selectBlock _ x = x

selectBlock' :: Options -> Block -> String -> [String] -> [(String, String)] -> Block
selectBlock' opts el _id classes attrs = if selectEl opts _id classes attrs then colorBlock opts el
                                         else if (not $ null $ opt_color opts) && (not $ elem "hidden" classes) then el else Null
       
colorBlock :: Options -> Block -> Block
colorBlock opts (Div (_id, classes, attrs) xs) = Div (colorEl opts _id classes attrs) xs
colorBlock opts (Header l (_id, classes, attrs) xs) = Header l (colorEl opts _id classes attrs) xs
colorBlock _ b = b

colorSpan :: Options -> Inline -> Inline
colorSpan opts (Span (_id, classes, attrs) xs) = (Span  (colorEl opts _id classes attrs) xs)
colorSpan _ s = s

colorEl :: Options -> String -> [String] -> [(String, String)] -> (String, [String], [(String, String)])
colorEl opts _id classes attrs = 
    if not $ null $ opt_color opts then (_id, "color":classes, ("color", opt_color opts):attrs)
    else (_id, classes, attrs)

-- select (blocks or spans) where (id = opt_id) or (class = always or class in opt_class) or ((date or update) = opt_date) or ((date or update) newer than opt_newer) or ((date or update) older than opt_older)
selectEl :: Options -> String -> [String] -> [(String, String)] -> Bool
selectEl opts _id classes attrs = 
             if elem "hidden" classes then False
             else if elem _id (opt_id opts)  then True
                  else if any (\c -> elem c ("always":(opt_classes opts))) classes then True
                       else if (not $ null $ opt_date opts) || (not $ null $ opt_newer opts) || (not $ null $ opt_older opts)
                            then selectByDate opts attrs
                            else if null classes && opt_includeEmpty opts then True
                                 else False


defaultDate :: Day
defaultDate = fromGregorian 1970 1 1

parseDate :: String -> Day
parseDate v = (fromMaybe  defaultDate $ parseTime defaultTimeLocale "%Y-%m-%d" v)


selectByDate :: Options -> [(String, String)] -> Bool
selectByDate opts attrs = 
    let date = case lookup "update" attrs of
                 Nothing -> case lookup "date" attrs of
                              Nothing -> defaultDate
                              Just d -> parseDate d
                 Just d -> parseDate d
    in if (not $ null $ opt_date opts) then ifExact (parseDate $ opt_date opts) date 
       else if (not $ null $ opt_older opts) then ifOlder (parseDate $ opt_older opts) date 
            else (ifNewer (parseDate $ opt_newer opts) date) 

ifNewer :: Day -> Day -> Bool
ifNewer newer date  = diffDays date newer > 0 -- date is newer than newer


ifOlder :: Day -> Day -> Bool
ifOlder older date  = ifNewer date older 

ifExact :: Day -> Day -> Bool
ifExact target date  = diffDays target date == 0 
