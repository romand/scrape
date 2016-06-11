module Scrape.JsonExp.Types where

import Import

import Data.Scientific (Scientific)

data JsonPart = JsonAny |
                JsonDeepAny |
                JsonObjectField Text |
                JsonArrayItem Scientific
              deriving (Show, Eq)

data JsonExp = JsonExp [JsonPart] deriving (Show, Eq)
