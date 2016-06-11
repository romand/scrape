module Scrape.SelectorExp.Eval where
import Scrape.SelectorExp.Types

import Import

import qualified Scrape.CssSelector.Eval as CS
import qualified Scrape.JsonExp.Eval as EJ

import Data.Text (strip)
import Text.XML.Cursor
import Text.Regex.PCRE ((=~))
import Data.Aeson (Value)

eval :: SelectorExp -> Cursor -> [Value]
eval (SelectorExp cs attr regex json) =
  CS.eval cs >=>
  evalAttr attr >=>
  evalRegex regex >=>
  (return . strip) >=>
  EJ.evalJson json

evalAttr :: AttrExp -> Cursor -> [Text]
evalAttr Content = return . concat . ($/ content)
evalAttr DeepContent = return . concat . ($// content)
evalAttr (Attr name) = laxAttribute name

evalRegex :: Maybe RegexExp -> Text -> [Text]
evalRegex Nothing = return
evalRegex (Just (RegexExp m rtxt)) = \txt ->
  let str = unpack txt
      rstr = unpack rtxt
  in if str =~ rstr
     then [pack $ getPart m (str =~ rstr :: (String, String, String))]
     else []

getPart :: RegexPart -> (String, String, String) -> String
getPart Before (before, _, _) = before
getPart Match (_, match, _) = match
getPart After (_, _, after) = after
