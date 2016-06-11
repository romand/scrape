{-# LANGUAGE FlexibleContexts #-}

module Scrape.JsonExp.Js2Json where

import Import hiding (fromList, catMaybes)

import Prelude (read)
import Data.ByteString.Lex.Integral (readHexadecimal, readOctal)
import Data.Aeson (Value(..), object, (.=))
import Data.Vector (fromList)
import Data.Maybe (catMaybes)
import Language.JavaScript.Parser (JSNode(..), Node(..), parse)
import qualified Data.ByteString.Char8 as B

js2json :: String -> Maybe Value
js2json script = case parse script "" of
                  Left _ -> Nothing
                  Right node -> jsNode node

readNum :: String
           -> (B.ByteString -> Maybe (Integer, B.ByteString))
           -> Maybe Value
readNum s f = Number . fromInteger . fst <$> f (B.pack s)

jsNode :: JSNode -> Maybe Value
jsNode (NT node _ _ ) =
  case node of
   (JSDecimal s) -> Just $ Number (read s)
   (JSHexInteger s) -> readNum s readHexadecimal
   (JSOctal s) -> readNum s readOctal
   (JSStringLiteral _ s) -> Just $ String (pack s)
   (JSLiteral "true") -> Just $ Bool True
   (JSLiteral "false") -> Just $ Bool False
   (JSLiteral "null") -> Just $ Null
   _ -> Nothing
jsNode (NN node) =
  case node of
   (JSArguments _ ns _) -> jsNodes ns
   (JSArrayLiteral _ ns _) -> Just $ Array $ fromList (jsNodeList ns)
   (JSBlock _ ns _) -> jsNodes ns
   (JSCallExpression _ _ ns _) -> jsNodes ns
   (JSCase _ n _ ns) -> jsNodes (n:ns)
   (JSCatch _ _ _ _ _ n) -> jsNode n
   (JSDefault _ _ ns) -> jsNodes ns
   (JSDoWhile _ n1 _ _ n2 _ _) -> jsNodes (n1:[n2])
   (JSExpression ns) -> jsNodes ns
   (JSExpressionBinary _ ns1 _ ns2) -> jsNodes (ns1 ++ ns2)
   (JSExpressionParen _ n _) -> jsNode n
   (JSExpressionPostfix _ ns _) -> jsNodes ns
   (JSExpressionTernary ns1 _ ns2 _ ns3) -> jsNodes (ns1 ++ ns2 ++ ns3)
   (JSFinally _ n) -> jsNode n
   (JSFor _ _ ns1 _ ns2 _ ns3 _ n) -> jsNodes (ns1 ++ ns2 ++ ns3 ++ [n])
   (JSForIn _ _ ns _ n1 _ n2) -> jsNodes (ns ++ [n1] ++ [n2])
   (JSForVar _ _ _ ns1 _ ns2 _ ns3 _ n) -> jsNodes (ns1 ++ ns2 ++ ns3 ++ [n])
   (JSForVarIn _ _ _ n1 _ n2 _ n3) -> jsNodes (n1:n2:[n3])
   (JSFunction _ _ _ ns _ n) -> jsNodes (ns ++ [n])
   (JSFunctionExpression _ _ _ ns _ n) -> jsNodes (ns ++ [n])
   (JSIf _ _ n _ ns1 ns2) -> jsNodes (n:ns1 ++ ns2)
   (JSLabelled _ _ n) -> jsNode n
   (JSMemberDot ns _ _) -> jsNodes ns
   (JSMemberSquare ns _ n _) -> jsNodes (ns ++ [n])
   (JSObjectLiteral _ ns _) -> jsObject ns
   (JSPropertyAccessor _ _ _ _ _ _) ->
     error "JSPropertyNameandValue outside JSObject literal"
   (JSPropertyNameandValue _ _ _) ->
     error "JSPropertyNameandValue outside JSObject literal"
   (JSReturn _ ns _) -> jsNodes ns
   (JSSourceElementsTop ns) -> jsNodes ns
   (JSSwitch _ _ n1 _ n2) -> jsNodes (n1:[n2])
   (JSThrow _ n) -> jsNode n
   (JSTry _ n ns) -> jsNodes (n:ns)
   (JSVarDecl _ ns) -> jsNodes ns
   (JSVariables _ ns _) -> jsNodes ns
   (JSWhile _ _ n1 _ n2) -> jsNodes (n1:[n2])
   (JSWith _ _ n _ ns) -> jsNodes (n:ns)
   _ -> Nothing

jsNodes :: [JSNode] -> Maybe Value
jsNodes nodes = case jsNodeList nodes of
                 [] -> Nothing
                 [x] -> Just x
                 xs -> Just $ Array $ fromList xs

jsNodeList :: [JSNode] -> [Value]
jsNodeList nodes = catMaybes $ map checkMinus $
                   zip nodes (NT e e e : nodes)
  where e = error "shouldn't happen"
        checkMinus (node, (NN (JSUnary "-" _))) = minus <$> jsNode node
        checkMinus (node, _) = jsNode node
        minus (Number x) = Number (negate x)
        minus x = x

jsObject :: [JSNode] -> Maybe Value
jsObject ns = Just $ object $ concat (map pair ns)
  where pair (NN (JSPropertyNameandValue n _ vs)) =
          toList $ (jsObjKey n .=) <$> jsNodes vs
        pair (NN (JSPropertyAccessor _ n _ _ _ v)) =
          toList $ (jsObjKey n .=) <$> jsNode v
        pair _ = []

jsObjKey :: JSNode -> Text
jsObjKey (NT n _ _) =
  pack $ case n of
          (JSIdentifier s) -> s
          (JSStringLiteral _ s) -> s
          (JSDecimal s) -> s
          (JSHexInteger s) -> s
          (JSOctal s) -> s
          _ -> error "unexpected json object key type"
jsObjKey _ = error "unexpected json object key type"
