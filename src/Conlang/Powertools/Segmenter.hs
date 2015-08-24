{-# LANGUAGE ViewPatterns, TransformListComp #-}
module Conlang.Powertools.Segmenter (
      segmenter
    , segmentize
    ) where

import Data.List
import GHC.Exts (sortWith, groupWith)


newtype Segmenter = S {
      segments :: [String]
    }


segmenter :: [String] -> Segmenter
segmenter input = S $ concat [x | x <- input
                                , then group by length x using groupWith
                                , then reverse
                                ]


segmentize :: Segmenter -> String -> [String]
segmentize s "" = []
segmentize s (segmentize1 s -> (segment, rest)) = segment : segmentize s rest

segmentize1 :: Segmenter -> String -> (String, String)
segmentize1 _ "" = error "segmentize1 called on empty string"
segmentize1 (S []) (c:str) = ("?", str)
segmentize1 (S (s:ss)) input = case s `stripPrefix` input of
    Just rest -> (s, rest)
    Nothing -> segmentize1 (S ss) input
