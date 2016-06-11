module Scrape.JsonExp.ParserSpec where

import Test

import Scrape.JsonExp.Types
import Scrape.JsonExp.Parser
import Data.Attoparsec.Text (parseOnly)

spec :: Spec
spec = do

  describe "json" $ do

    let s ~> je = parseOnly p_JsonExp s `shouldBe` (Right je)

    it "parses *" $
      "!json*" ~> JsonExp [JsonDeepAny]

    it "parses ?" $
      "!json?" ~> JsonExp [JsonAny]

    it "parses array indexing" $
      "!json[0]" ~> JsonExp [JsonArrayItem 0]


    it "parses object field" $
      "!json.product_id" ~> JsonExp [JsonObjectField "product_id"]

    it "parses chains" $
      "!json[0].productId" ~> JsonExp [JsonArrayItem 0,
                                       JsonObjectField "productId"]

