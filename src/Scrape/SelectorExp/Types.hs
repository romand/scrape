module Scrape.SelectorExp.Types where

import Import

import Scrape.JsonExp.Types
import Scrape.CssSelector.Types

data RegexPart = Match | Before | After deriving (Show, Eq)

data RegexExp = RegexExp RegexPart Text deriving (Show, Eq)

data AttrExp = Attr Text | Content | DeepContent deriving (Show, Eq)

data SelectorExp = SelectorExp { sexpCss :: CssExp
                               , sexpAttr :: AttrExp
                               , sexpRegex :: Maybe RegexExp
                               , sexpJson :: Maybe JsonExp
                               } deriving (Show, Eq)
