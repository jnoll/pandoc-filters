module Text.Pandoc.Filter.Color where
import Data.Maybe (fromMaybe)
import Text.Pandoc.Definition (Format(..), Block(Para))
import Text.Pandoc.Generic (bottomUp)
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.Printf (printf)

mkColorBackground :: String -> [Inline] -> [Inline]
mkColorBackground clr xs = (RawInline (Format "latex") $ (printf "\\colorbox{%s}{" (clr::String))):xs ++  [RawInline  (Format "latex") "}"]

mkColorText :: String -> [Inline] -> [Inline]
mkColorText clr xs = (RawInline (Format "latex") $ (printf "\\textcolor{%s}{" (clr::String))):xs ++  [RawInline  (Format "latex") "}"]




colorBackground :: Inline -> [Inline]
colorBackground sp@(Span (id, classes, attrs) xs) =
    if elem "bgcolor" classes then 
        let clr = fromMaybe "yellow" $ lookup "bgcolor" attrs
        in mkColorBackground clr xs
    else xs


colorForeground :: Inline -> [Inline]
colorForeground sp@(Span (id, classes, attrs) xs) =
      if elem "color" classes then 
          let clr = fromMaybe "red" $ lookup "color" attrs
          in colorBackground $ Span (id, classes, attrs) $ mkColorText clr xs
      else if elem "date" classes then
               mkColorText "blue" xs
           else if elem "updated" classes then
                    mkColorText "cyan" xs
                else colorBackground sp


colorInline :: Maybe Format -> Inline -> Inline
colorInline (Just f) (Emph xs)  -- XXXjn this shold obtain color from meta-data.
  | f == Format "latex" = Emph $ (RawInline (Format "latex") "\\textcolor{black}{"):xs ++  [RawInline  (Format "latex") "}"]
colorInline (Just f) sp@(Span (id, classes, attrs) xs)
  | f == Format "latex" = Span ("", ["placeholder"], []) $ colorForeground sp
colorInline _ x = x
                 
mkOption :: [(String, String)]  -> String -> String -> String
mkOption alist result cls  =
    case cls of
      "color" -> let clr = fromMaybe "red" $ lookup "color" alist
                 in printf "%s, fontcolor=%s" result clr
      "bgcolor" -> let clr = fromMaybe "yellow" $ lookup "bgcolor" alist
                 in printf "%s, backgroundcolor=%s" result clr
      "frame"  -> printf "%s, linecolor=black" result
      otherwise -> result

mkFrameOpts :: [String] -> [(String, String)] -> String
mkFrameOpts classes alist =
    foldl (mkOption alist) "linecolor=white" classes 

colorBlock :: Maybe Format -> Block -> Block
colorBlock fmt@(Just f) hd@(Header lvl attr@(id, classes, alist) xs)
  | f == Format "latex" =
      if elem "color" classes then 
          let clr = fromMaybe "red" $ lookup "color" alist
          in Header lvl attr $ mkColorText clr xs
      else hd
colorBlock fmt@(Just f) div@(Div attr@(id, classes, alist) xs)
  | f == Format "latex" =
      let opts = mkFrameOpts classes alist
      in Div ("", ["placeholder"], []) $ (RawBlock (Format "latex") $ printf "\\begin{mdframed}[%s]" opts):xs ++ [RawBlock (Format "latex") "\\end{mdframed}"]

colorBlock _ x = x

cb :: Maybe Format -> Pandoc -> Pandoc
cb f = bottomUp (colorBlock f)

ci :: Maybe Format -> Pandoc -> Pandoc
ci f = bottomUp (colorInline f)

colorAll :: Maybe Format -> Pandoc -> Pandoc
colorAll f = (ci f) . (cb f) 
