module Scrape.ParserUtils where

import Import

import Data.Attoparsec.Text
import Data.Char (isAlphaNum)

spaces :: Parser ()
spaces = skipMany (char ' ')

p_ident :: Parser Text
p_ident = takeWhile1 isIdent

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_' || c == '-'

p_str :: Char -> Parser Text
p_str sep =
  char sep *> lit <* char sep
  where lit = pack <$> many' (
              choice [ char '\\' *> anyBut "\n\r\f"
                     , anyBut (sep:"\n\r\f") ]
              )

anyBut :: String -> Parser Char
anyBut cs = satisfy (not . (`elem` cs))
