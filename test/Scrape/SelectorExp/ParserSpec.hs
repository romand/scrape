module Scrape.SelectorExp.ParserSpec where

import Test

import Scrape.SelectorExp.Types
import Scrape.SelectorExp.Parser
import Data.Attoparsec.Text
import Data.Either (isRight)

spec :: Spec
spec = do

  describe "regex" $ do

    let s ~> r = parseOnly p_RegexExp s `shouldBe` (Right r)

    it "parses regex between slashes" $
      "=~ /m{3,4}[A-Z0-9]?|hello_-*#~/" ~>
      RegexExp Match "m{3,4}[A-Z0-9]?|hello_-*#~"

    it "parses regex between tubes" $
      "=~ before |//\\\\o//\\\\|" ~> RegexExp Before "//\\o//\\"

  describe "selector" $ do

    let ok s = parseOnly p_SelectorExp s `shouldSatisfy` isRight

    it "parses plain css selector" $
      ok "#id .class.class1 [attr]"

    it "parses css with @attr" $
      ok "#id @content"

    it "parses css with regex" $
      ok "#id =~ before |mumu|"

    it "parses css with @attr and regex" $
      ok "#id @content =~ after /US /"

    it "parses css with json" $
      ok "script !json[0]"

