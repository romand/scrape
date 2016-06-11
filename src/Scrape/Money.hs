module Scrape.Money where

import Import

import Scrape.ParserUtils
import Data.Attoparsec.Text

data Currency = USD | GBP | EUR deriving (Eq, Show)

data Money = Money { currency :: Currency
                   , nCents :: Int
                   } deriving (Eq, Show)

p_Currency :: Parser Currency
p_Currency = choice [ string "USD" *> return USD
                    , string "GBP" *> return GBP
                    , string "EUR" *> return EUR
                    , char '$' *> return USD
                    , char '£' *> return GBP
                    , char '€' *> return EUR
                    ]

p_Money :: Parser Money
p_Money = do
  (c, amount) <- choice [ (,) <$> (spaces *> p_Currency)
                              <*> (spaces *> double)
                        , (flip (,)) <$> (spaces *> double)
                                     <*> (spaces *> p_Currency) ]
  return $! Money c (round $ amount * 100)
