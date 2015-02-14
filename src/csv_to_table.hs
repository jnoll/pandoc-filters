import Text.Pandoc.Filter.Tables (formatTableBlock)
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter formatTableBlock
