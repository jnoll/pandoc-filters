{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Filter.SelectDiv (selectDiv) where
import Data.Maybe (maybe, fromJust, fromMaybe)
import Text.Pandoc

selectDiv :: Maybe String -> Maybe String -> Block -> IO Block
selectDiv div_id div_class _div@(Div (_id, classes, namevals) _) =
    return $ if (maybe False (\i -> i == _id) div_id)  || elem (fromMaybe "always" div_class) classes then _div else Null
selectDiv _ _ x = return x
