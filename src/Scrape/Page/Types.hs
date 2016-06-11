module Scrape.Page.Types where

import Import

import Scrape.SelectorExp.Types
import System.FilePath (takeBaseName)

data Expr = Expr { exprField :: Text
                 , exprSelector :: SelectorExp } deriving (Show, Eq)

data PageExp = PageExp { pageUrlFilter :: Text
                       , pageExprs :: [Expr] }
             deriving (Show, Eq)

type PageName = Text

data Page = Page { pagePath :: Maybe FilePath
                 , pageCode :: Text
                 , pageExp :: PageExp }
            deriving (Show, Eq)

pageName :: Page -> Maybe PageName
pageName page = pack . takeBaseName <$> pagePath page
