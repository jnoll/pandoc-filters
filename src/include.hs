import Text.Pandoc.Filter.Include (include)
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter include
