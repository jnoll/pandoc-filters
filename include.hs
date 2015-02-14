import Text.Pandoc.Include (include)
import Text.Pandoc
    
main :: IO ()
main = toJSONFilter include
