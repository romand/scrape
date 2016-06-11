{-# LANGUAGE QuasiQuotes #-}

module Scrape.JsonExp.Js2JsonSpec where

import Test
import Data.Aeson (Value)
import Data.Aeson.QQ
import Scrape.JsonExp.Js2Json

yields :: String -> Value -> Expectation
yields js json = js2json js `shouldBe` Just json

spec :: Spec
spec = do

  describe "js2json" $ do

    it "parses simple object" $
      "x={a: 1}" `yields` [aesonQQ|{a: 1}|]

    it "parses unary -" $
      "x={a: -1}" `yields` [aesonQQ|{a: -1}|]

    it "parses smth nontrivial" $
      "function(z, b) { while(1) { x={a: -1, 3: [[[]]]}; b += 2 } }"
      `yields` [aesonQQ|[1, [{a: -1, 3: [[[]]]}, 2]]|]

    it "parses floats" $
      "function(z, b) {  x={a: -133.0} }"
      `yields` [aesonQQ|{a: -1.33e2}|]
