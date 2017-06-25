import Text.Pandoc.Filter.Color (colorInline, colorAll)
import Text.Pandoc.JSON (toJSONFilter)
    

main :: IO ()
main = toJSONFilter colorAll
