module Scrape.CssSelector.Eval where
import Scrape.CssSelector.Types

import Import
import Text.XML.Cursor
import Data.Text (splitOn)

idAxis :: Axis
idAxis cursor = [cursor]

eval :: CssExp -> Axis
eval [] = idAxis
eval ((CssSelector rel name attrs):ss) =
  relA rel >=>
  nameA name >=>
  attrsA attrs >=>
  eval ss

relA :: RelPrev -> Axis
relA Descendant = descendant
relA Child = child
relA Next = take 1 . (followingSibling >=> anyElement)
relA Sibling = followingSibling

nameA :: Maybe Text -> Axis
nameA Nothing = anyElement
nameA (Just name) = laxElement name

attrsA :: [TagAttr] -> Axis
attrsA [] = idAxis
attrsA (t:ts) = attrA t >=> attrsA ts

attrA :: TagAttr -> Axis
attrA (TagAttr name Nothing) = check $ \cursor ->
  length (laxAttribute name cursor) == 1
attrA (TagAttr name (Just (rel, pat))) = check $ \cursor ->
  case laxAttribute name cursor of
   [value] -> checkAttr rel pat value
   _ -> False

checkAttr :: AttrRel -> Text -> Text -> Bool
checkAttr PrefixMatch = isPrefixOf
checkAttr SuffixMatch = isSuffixOf
checkAttr SubstringMatch = isInfixOf
checkAttr Equal = (==)
checkAttr Includes = \pat value -> pat `elem` (splitOn " " value)
checkAttr DashMatch = \pat value -> pat `elem` (splitOn "-" value)
