module Scrape.JsonExp.Eval where

import Import

import Scrape.JsonExp.Types
import Scrape.JsonExp.Js2Json (js2json)
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as H
import Data.Vector ((!?))
import Data.Scientific (toBoundedInteger, floatingOrInteger)

evalJson :: (Maybe JsonExp) -> Text -> [Value]
evalJson Nothing txt = [String txt]
evalJson (Just (JsonExp parts)) script =
  evalParts parts (js2json $ unpack script)

evalParts :: [JsonPart] -> Maybe Value -> [Value]

evalParts _ Nothing  = []
evalParts [] v = toList v

evalParts (JsonObjectField field : rest) (Just (Object h)) =
  evalParts rest $ H.lookup field h
evalParts (JsonObjectField _ : _) _ = []

evalParts (JsonArrayItem ind : rest) (Just (Object h)) =
  let field = either show show (floatingOrInteger ind :: Either Double Integer)
  in evalParts rest $ H.lookup (pack field) h
evalParts (JsonArrayItem ind : rest) (Just (Array a)) =
  evalParts rest ((a !?) =<< toBoundedInteger ind)
evalParts (JsonArrayItem _ : _) _ = []

evalParts (JsonAny : rest) (Just (Object h)) =
  concatMap (evalParts rest . Just) (H.elems h)
evalParts (JsonAny : rest) (Just (Array a)) =
  concatMap (evalParts rest . Just) (toList a)
evalParts (JsonAny : _) _ = []

evalParts parts@(JsonDeepAny : rest) value@(Just (Object h)) =
  evalParts rest value ++
  concatMap (evalParts parts . Just) (H.elems h)
evalParts parts@(JsonDeepAny : rest) value@(Just (Array a)) =
  evalParts rest value ++
  concatMap (evalParts parts . Just) (toList a)
evalParts (JsonDeepAny : rest) value@(Just _) = evalParts rest value
