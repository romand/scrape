module Scrape.CssSelector.Types where

import Import

type CssExp = [CssSelector]

data CssSelector = CssSelector { csRelPrev :: RelPrev
                               , csName :: Maybe Text
                               , csAttrs :: [TagAttr] }
                   deriving (Show, Read, Eq, Ord)

data RelPrev = Descendant | Child | Next | Sibling
             deriving (Enum, Show, Read, Eq, Ord)

data TagAttr = TagAttr { attrName :: Text
                       , attrVal :: Maybe (AttrRel, Text) }
             deriving (Show, Read, Eq, Ord)

data AttrRel = PrefixMatch | SuffixMatch | SubstringMatch |
               Equal | Includes | DashMatch
             deriving (Enum, Show, Read, Eq, Ord)
