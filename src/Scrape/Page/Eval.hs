module Scrape.Page.Eval where
import Scrape.Page.Types

import Import
import qualified Scrape.SelectorExp.Eval as SE
import Text.XML.Cursor
import Data.Aeson (Value)

eval :: PageExp -> Cursor -> [(Text, Value)]
eval se cursor = concat $ map (\expr -> evalExpr expr cursor) (pageExprs se)

evalExpr :: Expr -> Cursor -> [(Text, Value)]
evalExpr (Expr name sel) =
  take 1 . SE.eval sel >=> \val -> [(name, val)]
