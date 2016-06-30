{-# LANGUAGE DeriveDataTypeable #-}
-- following for 'here' docs
{-# LANGUAGE QuasiQuotes #-} 
import Text.Pandoc.Filter.SelectElement (selectEl)
import Text.Pandoc.JSON
import System.Console.CmdArgs

data Options = Options {
      opt_id :: [String]               -- Required
    , opt_class :: [String]
} deriving (Data, Typeable, Show)

defaultOptions :: Options
defaultOptions = Options { 
                   opt_id = [] &= help "div or header ids to select" &= explicit &= name "id"
                 , opt_class = [] &= help "div or header classes to select" &= explicit &= name "class"
                 }
                 &= summary "select_element v0.1, (C) 2016 John Noll"
                 &= program "main"


run :: Options -> IO ()
run opts = toJSONFilter $ selectEl  (opt_id opts) ("always":(opt_class opts))

main :: IO ()
main = do
  args <- cmdArgs defaultOptions
  run args

