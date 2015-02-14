{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{- 
   Get block contents, either from file specified in attributes, or block itself.
-}

module Text.Pandoc.Filter.BlockContents (getBlockContents, getBlockContentsString) where
import System.IO
import System.IO.Error (IOError, try)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL


findFile :: String -> IO String
findFile f = do
  result <- try $ openFile f ReadMode  :: IO (Either IOError Handle) 
  case result of
    Left e  -> return $ "../" Prelude.++ f
    Right x -> do hClose x
                  return f

getFileContents :: FilePath -> IO BL.ByteString
getFileContents = BL.readFile

-- Get block contents from specified file or block if not present.
getBlockContents :: [(String, String)] -> String -> IO ([(String, String)], BL.ByteString)
getBlockContents namevals contents = 
    case lookup "include" namevals of
      Just f -> do p <- findFile f
                   c <- getFileContents p
                   return $ (filter (\(n, v) -> n /= "include") namevals, c)
      Nothing -> return $ (namevals, BL.pack contents)


getBlockContentsString :: [(String, String)] -> String -> IO ([(String, String)], String)
getBlockContentsString namevals contents = do
  (nv, cs) <- getBlockContents namevals contents
  return (nv, BL.unpack cs)