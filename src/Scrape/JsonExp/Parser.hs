module Scrape.JsonExp.Parser where

import Import

import Scrape.JsonExp.Types
import Scrape.ParserUtils
import Data.Attoparsec.Text

p_JsonExp :: Parser JsonExp
p_JsonExp =
  string "!json" *> (JsonExp <$> many' (p_JsonObjField <|>
                                        p_JsonArrItem <|>
                                        p_JsonAny <|>
                                        p_JsonDeepAny))

p_JsonObjField :: Parser JsonPart
p_JsonObjField = char '.' *> (JsonObjectField <$> p_ident)

p_JsonArrItem :: Parser JsonPart
p_JsonArrItem =
  JsonArrayItem <$> (char '[' *> scientific <* char ']')

p_JsonAny :: Parser JsonPart
p_JsonAny = char '?' *> return JsonAny

p_JsonDeepAny :: Parser JsonPart
p_JsonDeepAny = char '*' *> return JsonDeepAny
