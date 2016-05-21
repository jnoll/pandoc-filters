{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Filter.Include (include) where
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Utils (strip)
import System.IO 
import System.FilePath.Posix  (combine)
import System.Directory (doesFileExist)
                    
findFile :: [FilePath] -> String -> IO (Maybe FilePath)
findFile [] _ = return Nothing
findFile (d:dirs) f = do
  b <- doesFileExist $ combine d f
  if b then return $ Just f else findFile dirs f

isNot :: String ->  (String, String)  -> Bool
isNot key (n, _) = key /= n

includeCodeBlock :: String -> [String] -> [(String, String)] -> String -> IO Block
includeCodeBlock _id classes namevals file = do
    p <- findFile [".", ".."] file
    case p of
      Just f -> do                
             c <- readFile f
             let (Pandoc _ bs) = handleError $ readMarkdown def c
             return $ CodeBlock (_id, classes, filter (isNot "include") namevals) $ strip c
      Nothing -> return (CodeBlock (_id, classes, filter (isNot "include") namevals)  ("file " ++ file ++ " not found"))


includePandoc :: String -> [String] ->  [(String, String)] -> String -> IO Block
includePandoc _id classes namevals file = do
    p <- findFile [".", ".."] file
    case p of
      Just f -> do
             c <- readFile f
             let (Pandoc _ bs) = handleError $ readMarkdown def c
             return (Div (_id, classes, filter (isNot "include") namevals)  bs)
      Nothing -> return $ Div (_id, classes, filter (isNot "include") namevals)  [Plain [Strong $ [Str ("file " ++ file ++ " not found")]]]


include :: Block -> IO Block
include cb@(CodeBlock (_id, classes, namevals) contents) =
  case lookup "include" namevals of 
    Just file -> includeCodeBlock _id classes namevals file
    Nothing -> return cb

include _div@(Div (_id, classes, namevals) _) =
  case lookup "include" namevals of 
    Just file -> if elem "code" classes then
                     includeCodeBlock _id (filter (\x -> x /= "code") classes) namevals file
                 else
                     includePandoc _id classes namevals file
    Nothing   -> return _div
include x = return x
