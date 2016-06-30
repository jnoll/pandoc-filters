{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Filter.Subst (substitute) where
import qualified Data.List (lookup)
import Text.Pandoc

substitute :: [(String, String)] -> Inline -> Inline
substitute words s@(Str x) = case lookup x words of
                             Just w -> Str w
                             otherwise -> s
substitute _ x = x
