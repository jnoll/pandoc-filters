import Text.Pandoc.Filter.Beamer (formatDiv)
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter formatDiv
