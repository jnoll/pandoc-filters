{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
import Text.Pandoc.Filter.BibEntry (bibentry)
import Text.Pandoc.JSON
import Text.CSL.Input.Bibutils (readBiblioFile, readBiblioString, BibFormat(..))
import Text.CSL.Parser (readCSLFile)
import WithCli

data Options = Options {
      opt_bib :: String
    , opt_csl :: String
} deriving (Show, Generic, HasArguments)

run :: Options -> IO ()
run opts = do
  bib <- readFile $ opt_bib opts
  refs <- readBiblioString Bibtex bib
  sty <- readCSLFile Nothing $ opt_csl opts
  toJSONFilter $ bibentry sty refs

main :: IO ()
main = withCliModified [ RenameOption "opt_bib" "bib"
                       , AddShortOption "bib" 'b' -- allow '-b' in addition to '--bib'
                       , RenameOption "opt_csl" "csl"
                       , AddShortOption "csl" 'c' -- allow '-c' as well as '--csl'
                       , AddShortOption "csl" 's' -- allow '-s' as well as '--csl'
                       ] run


