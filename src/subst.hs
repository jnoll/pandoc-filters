{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
import Data.Maybe (fromMaybe)
import Text.Pandoc.Filter.Subst (substitute)
import Text.Pandoc.JSON
import Text.Pandoc.Filter.CSVTable (getCSV)
import WithCli

data Options = Options {
      opt_subfiles :: [String]   -- word to word mappings
    , opt_delim    :: Maybe String
} deriving (Show, Generic, HasArguments)

run :: Options -> IO ()
run opts = do
  contents <- readFile $ head $ opt_subfiles opts -- XXX what if empty or more than one?
  let (_, rows) = getCSV (head $ fromMaybe "|" (opt_delim opts))  contents 
      ws = map (\(a:b:_) -> (a, b)) rows
  toJSONFilter $ substitute ws

main :: IO ()
main = withCliModified [ UseForPositionalArguments "opt_subfiles" "filename"
                       , RenameOption "opt_delim" "sep"
                       , AddShortOption "sep" 's'
                       ] run
