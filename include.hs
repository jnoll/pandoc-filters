{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--import Control.Applicative ((<$>), (<*>))
import BlockContents
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.JSON
import Data.Maybe (fromJust, fromMaybe)
import System.IO
import System.IO.Error (IOError, try)

findFile :: String -> IO String
findFile f = do
  result <- try $ openFile f ReadMode  :: IO (Either IOError Handle) 
  case result of
    Left e  -> return $ "../" Prelude.++ f
    Right x -> do hClose x
                  return f

keep :: String ->  (String, String)  -> Bool
keep key (n, _) = key /= n

includeCodeBlock :: String -> [String] -> [(String, String)] -> String -> IO Block
includeCodeBlock id classes namevals file = do  
  p <- findFile file
  c <- readFile p
  return $ CodeBlock (id, classes, filter (keep "include") namevals) c

includePandoc :: String -> [String] ->  [(String, String)] -> String -> IO Block
includePandoc id classes namevals file = do
  p <- findFile file
  c <- readFile p
  let (Pandoc _ bs) = readMarkdown def c
  return (Div (id, classes, filter (keep "include") namevals)  bs)


include :: Block -> IO Block
include cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of 
    Just file -> includeCodeBlock id classes namevals file
    Nothing -> return cb

include div@(Div (id, classes, namevals) _) =
  case lookup "include" namevals of 
    Just file -> if elem "code" classes then
                     includeCodeBlock id (filter (\x -> x /= "code") classes) namevals file
                 else
                     includePandoc id classes namevals file
    Nothing   -> return div
include x = return x

main :: IO ()
main = toJSONFilter include
