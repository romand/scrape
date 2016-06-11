module Scrape.SelectorExp.Parser where
import Scrape.SelectorExp.Types

import Import

import Data.Attoparsec.Text
import Scrape.ParserUtils
import Scrape.CssSelector.Parser
import Scrape.JsonExp.Parser

p_DeepContent :: Parser AttrExp
p_DeepContent = string "!deep-content" *> return DeepContent

p_Attr :: Parser AttrExp
p_Attr = char '@' *> (Attr <$> p_ident)

p_RegexPart :: Parser RegexPart
p_RegexPart = string "before" *> return Before <|>
              string "after" *> return After <|>
              return Match

p_RegexExp :: Parser RegexExp
p_RegexExp = RegexExp
             <$> (string "=~" *> spaces *> p_RegexPart)
             <*> (spaces *> (p_str '/' <|> p_str '|'))

p_SelectorExp :: Parser SelectorExp
p_SelectorExp =
  SelectorExp
  <$> p_CssExp
  <*> (spaces *> option Content (p_Attr <|> p_DeepContent))
  <*> (spaces *> option Nothing (Just <$> p_RegexExp))
  <*> (spaces *> option Nothing (Just <$> p_JsonExp))


