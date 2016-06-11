{-# LANGUAGE FlexibleContexts #-}

module Scrape.Page.Parser where
import Scrape.Page.Types

import Import

import Scrape.SelectorExp.Parser
import Scrape.ParserUtils

import Data.Attoparsec.Text
import Data.Char (isSpace)

p_Expr :: Parser Expr
p_Expr = Expr
         <$> p_ident
         <*> (spaces *> char '=' *> spaces *> p_SelectorExp)

p_PageExp :: Parser PageExp
p_PageExp = do
  urlFilter <- p_str '|'
  skipWhile isSpace
  exprs_ <- char '{' *>
            (skipWhile isSpace *> many1' (p_Expr <* skipWhile isSpace))
            <* char '}'
  return $! PageExp { pageUrlFilter = urlFilter , pageExprs = exprs_ }

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f (Left x) = Left $! f x
mapLeft _ (Right x) = Right x

parsePage :: Maybe FilePath -> Text -> Either Text Page
parsePage path code = do
  pageExp' <- mapLeft fromString $! parseOnly p_PageExp code
  return $! Page { pagePath = path, pageCode = code, pageExp = pageExp' }

loadPage :: FilePath -> IO (Either Text Page)
loadPage path = do
  code <- readFile path
  return $! parsePage (Just path) code

loadPages :: [FilePath] -> IO ([Text], [Page])
loadPages paths = partitionEithers <$> mapM loadPage paths
