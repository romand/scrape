module Scrape.JsonExp.EvalSpec where

import Test

import Scrape.JsonExp.Parser
import Scrape.JsonExp.Eval
import Data.Attoparsec.Text (parseOnly)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as L

ej :: Text -> Text -> [L.ByteString]
ej s js = map encode $
          evalJson (Just $ fromRight $ parseOnly p_JsonExp s) js

spec :: Spec
spec = do

  describe "evalJson" $ do

    it "ignores malformed js" $
      ej "!json.a" "(*&^%&*" `shouldBe` []

    it "parses simple field" $
      ej "!json.a" "x={a: 1}" `shouldBe` ["1"]

    it "parses simple array" $
      ej "!json[1]" "x=['', {}, 123e4]" `shouldBe` ["{}"]

    it "index object by number" $
      ej "!json[1]" "x= { z: '', 3: {}, 1: 1.23e+4}" `shouldBe` ["12300"]

    it "index object by float, empty string as value" $
      ej "!json[1.2E+2]" "x= { 120: '' }" `shouldBe` ["\"\""]

    it "nested struct" $
      ej "!json.a[0].true" "x ={a: [{true: null}]}" `shouldBe` ["null"]

    it "renders json" $
      ej "!json.a[0]" "x ={a: [{true: null}]}" `shouldBe` ["{\"true\":null}"]

    it "supports ? for objects" $
      ej "!json?.id" "x = { id: 1, a: {id: 2}, b: {c: {id: 3}} }" `shouldBe`
      ["2"]

    it "supports ? for arrays" $
      ej "!json?.id" "x = [ 1, {id: 2}, {c: {id: 3}} ]" `shouldBe`
      ["2"]

    it "supports *" $
      ej "!json*.id" "x = [ 1, {id: 2}, {c: false ? {id: 3} : 0} ]" `shouldBe`
      ["2", "3"]

    it "supports * for scalars" $
      ej "!json*" "x = 1" `shouldBe` ["1"]
