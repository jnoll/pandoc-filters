{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
--import Control.Applicative ((<$>), (<*>))
module Text.Pandoc.Filter.Beamer (formatDiv) where
import Data.String.Here (i)
import Data.Maybe (fromMaybe)
import Text.Pandoc.Definition

formatDiv :: Block -> IO [Block]
formatDiv _div@(Div (_id, classes, namevals) b) = 
    if elem "bg" classes then 
        let title = fromMaybe "(title)" $ lookup "title" namevals
            block_begin = case lookup "img" namevals of
                    Just img' -> [i|\\end{frame} % close current slide env
{ % open new block
  \\setbeamertemplate{background canvas}{\\includegraphics[width=\\paperwidth]{${img'}}} 
  \\setbeamertemplate{footline}{}
  \\begin{frame}{${title}} % open new slide env
|]
                    otherwise -> "{"
            block_end = [i|\\end{frame} % end this div slide
}
\\begin{frame} %|] -- this is bogus: will create a blank slide.
        in return $ [(RawBlock (Format "latex") block_begin)] ++ b ++ [(RawBlock (Format "latex") block_end)]
    else return $ [(RawBlock (Format "latex") "{")] ++ b ++ [(RawBlock (Format "latex") "}")]

formatDiv x = return [x]

