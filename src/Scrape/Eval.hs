{-# LANGUAGE FlexibleContexts #-}

module Scrape.Eval where

import Import

import Scrape.Page.Types
import Scrape.Page.Eval (eval)

import Text.Regex.PCRE ((=~))
import Text.HTML.DOM as DOM (readFile)
import Text.XML.Cursor (fromDocument)
import Data.Aeson (ToJSON(..), Value, object, pairs, (.=))

newtype ScrapeResult = ScrapeResult [(Text, Value)]

evalPages :: String -> FilePath -> [Page] -> IO (Either Text ScrapeResult)
evalPages url path pages = do
  doc <- DOM.readFile path
  let pages' = filter ((url =~) . unpack . pageUrlFilter . pageExp) pages
      page' = unsafeHead pages'
      root = fromDocument doc
      res = eval (pageExp page') root
  if (null pages')
    then return $! Left "no url match"
    else return $! Right $! ScrapeResult $! ["page" .= pageName page'] ++ res

instance ToJSON ScrapeResult where
  toJSON (ScrapeResult kvs) = object $ map (uncurry (.=)) kvs
  toEncoding (ScrapeResult kvs) = pairs $ fold $ map (uncurry (.=)) kvs
