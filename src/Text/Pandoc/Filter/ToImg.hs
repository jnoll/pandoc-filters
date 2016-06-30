{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Filter.ToImg (toImg) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe (fromJust, fromMaybe)
import System.Directory (copyFile, doesFileExist, getDirectoryContents, getTemporaryDirectory)
import System.Exit
import System.FilePath
import System.FilePath.Posix  (combine)
import System.IO 
import System.IO.Temp (withTempFile)
import System.Process
import Text.Pandoc

-- Most of this cribbed from Ditaa gitit plugin.                    
findFile :: [FilePath] -> String -> IO (Maybe FilePath)
findFile [] _ = return Nothing
findFile (d:dirs) f = do
  b <- doesFileExist $ combine d f
  if b then return $ Just f else findFile dirs f

isNot :: String ->  (String, String)  -> Bool
isNot key (n, _) = key /= n

maybeHead :: [a] -> Maybe a
maybeHead = foldr ((Just .) . const) Nothing

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb mact = mb >>= \b -> unless b mact

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String -> String
uniqueName prefix contents =  (++) prefix $ showDigest $ sha1 $ fromString contents

-- | Find the XXX.jar file in the given directory
findJar :: String -> FilePath -> IO FilePath
findJar basename dir = fmap (fromMaybe (error $ "Could not locate the .jar file in the directory " ++ dir) . maybeHead .
                         filter (basename `isInfixOf`) . filter (".jar" `isSuffixOf`)) $ getDirectoryContents dir


plantuml :: FilePath -> String -> String -> IO ()
plantuml outfile libdir contents = 
  unlessM (doesFileExist outfile) $ do
     -- 0) Establish a temporary file name to store the data into before running ditaa
    tmp_dir <- getTemporaryDirectory
    withTempFile tmp_dir "contents.puml" $ \temp_file temp_file_h -> do
        -- 1) Setup input file by writing contents to it:
        hPutStrLn temp_file_h contents
        hClose temp_file_h
  
        -- 2) Run plantuml to turn into an equivalently named .png:
        jar <- findJar "plantuml" libdir
        let options = ["-jar"
                      , libdir </> jar
                      ] ++ [temp_file]
        (ec, _, stderr_out) <- readProcessWithExitCode "java" options ""
        if ec == ExitSuccess
           then copyFile (replaceExtension temp_file ".png") outfile
           else error $ "Error running plantuml from " ++ jar ++ ": " ++ stderr_out

  
toImg :: Block -> IO Block
toImg cb@(CodeBlock (_id, classes, namevals) contents) =
    if elem "plantuml" classes then
        do
          let outfile =  uniqueName "input" contents <.> "png"
          let libdir = "/home/jnoll" </> "lib"
          plantuml outfile libdir contents
          return $ Para [Image (_id, classes, namevals) [] (outfile, "")]
    else return cb

toImg x = return x
