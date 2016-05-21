{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
import Text.Pandoc.Filter.SelectDiv (selectDiv)
import Text.Pandoc.JSON
import WithCli

data Options = Options {
      opt_id :: Maybe String               -- Required
    , opt_class :: Maybe String
} deriving (Show, Generic, HasArguments)

run :: Options -> IO ()
run opts = toJSONFilter $ selectDiv  (opt_id opts) (opt_class opts)

main :: IO ()
main = withCliModified [ AddShortOption "id" 'i' -- allow '-i' in addition to '--id'
                       , RenameOption "opt_id" "id"
                       , AddShortOption "class" 'c' 
                       , RenameOption "opt_class" "class"
                       ] run
