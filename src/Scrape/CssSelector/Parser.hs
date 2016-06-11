module Scrape.CssSelector.Parser where

import Import

import Scrape.CssSelector.Types
import Scrape.ParserUtils

import Data.Attoparsec.Text
import Control.Arrow (left)

-- http://www.w3.org/TR/css3-selectors/#w3cselgrammar

parseCssSel :: Text -> Either Text CssExp
parseCssSel = left pack . parseOnly (p_CssExp <* endOfInput)

p_CssExp :: Parser CssExp
p_CssExp = many' p_CssSelector

p_CssSelector :: Parser CssSelector
p_CssSelector = do
  rel' <- p_RelPrev
  spaces
  name' <- p_Name
  attrs' <- p_TagAttrs
  when (rel' == Descendant && name' == Nothing && null attrs') $
    fail "empty selector"
  return $! CssSelector rel' name' attrs'

p_RelPrev :: Parser RelPrev
p_RelPrev = option Descendant $ rel <$> choice [ spaces *> char '>'
                                               , spaces *> char '+'
                                               , spaces *> char '~'
                                               , char ' ' <* spaces
                                               ]
  where rel '>' = Child
        rel '+' = Next
        rel '~' = Sibling
        rel ' ' = Descendant
        rel _ = error "should not happen"

p_Name :: Parser (Maybe Text)
p_Name = option Nothing $ choice [ char '*' *> return Nothing
                                 , Just <$> p_ident ]

p_TagAttrs :: Parser [TagAttr]
p_TagAttrs = many' (p_IdAttr <|> p_ClassAttr <|> p_TagAttr)

p_IdAttr :: Parser TagAttr
p_IdAttr = do
  id' <- char '#' *> p_ident
  return $! TagAttr "id" (Just (Equal, id'))

p_ClassAttr :: Parser TagAttr
p_ClassAttr = do
  class' <- char '.' *> p_ident
  return $! TagAttr "class" (Just (Includes, class'))

p_TagAttr :: Parser TagAttr
p_TagAttr = TagAttr
            <$> (char '[' *> spaces *> p_ident)
            <*> (spaces *> p_AttrVal <* spaces <* char ']')

p_AttrVal :: Parser (Maybe (AttrRel, Text))
p_AttrVal = option Nothing $ Just <$> choice (map p_AttrVal' attrRels)

p_AttrVal' :: (AttrRel, Text) -> Parser (AttrRel, Text)
p_AttrVal' (rel, str) = (,)
                        <$> (string str *> spaces *> return rel)
                        <*> choice [ p_ident
                                   , p_str '"'
                                   , p_str '\'']

attrRels :: [(AttrRel, Text)]
attrRels = [ (Equal, "=")
           , (PrefixMatch, "^=")
           , (SuffixMatch, "$=")
           , (Includes, "~=")
           , (DashMatch, "|=")
           , (SubstringMatch, "*=")]
