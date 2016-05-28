{- Convert <div class="bibentry"  id="citation-tag"></div> to CSL-formatted Reference for Pandoc.
   Useful for CV.
 -}
module Text.Pandoc.Filter.BibEntry (bibentry) where
import Text.Pandoc
--import Text.Pandoc.Definition (Inline(..), Block(..))
import Text.Pandoc.JSON
import Text.Pandoc.Writers.Native (writeNative)
import Text.CSL
import Text.CSL.Proc (citeproc, processBibliography, procOpts)
import Text.CSL.Output.Plain (renderPlain, renderPlain)
import Text.CSL.Output.Pandoc (renderPandoc, renderPandoc')
import Text.CSL.Style (Formatted(..))

toPandoc :: Style -> Formatted -> Block
toPandoc style ref = renderPandoc' style (ref, "foo")

-- convert <div .bibentry #citation-tag></div>
bibentry :: Style -> [Reference] -> Block -> IO [Block]
bibentry style bib _div@(Div (_id, classes, namevals) _) =
    if elem "bibentry" classes 
    then return $ map (toPandoc style) $ bibliography $ citeproc procOpts style bib $ [[emptyCite { citeId = _id }]]
    else return [_div]
--return $ RawBlock (Format "latex") _id 

bibentry _ _ x = return [x]
