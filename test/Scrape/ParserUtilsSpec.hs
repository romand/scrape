module Scrape.ParserUtilsSpec where

import Test

import Scrape.ParserUtils
import Data.Attoparsec.Text

spec :: Spec
spec = do

  describe "p_str" $ do

    let s ~> s' = parseOnly (p_str '\'' <|> p_str '"') s `shouldBe` (Right s')

    it "parses single quoted string" $
      "'sd\"\\' '" ~> "sd\"' "

    it "parses double quoted string" $
      "\"\\\" '\\\\ \"" ~> "\" '\\ "
