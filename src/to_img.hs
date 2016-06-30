import Text.Pandoc.Filter.ToImg (toImg)
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter toImg
